
#-------------- mikes idea years_of_sales_data

# create grid with all clients for years 1996 - 2016
clients_years_grid <- expand.grid(year_n = seq(1, max(clients$year_n, na.rm = T), by = 1), RC.Account.Number = unique(clients$RC.Account.Number))
df.plot <- left_join(clients_years_grid, clients, by = c('RC.Account.Number', 'year_n'))

# note that a lot of vars aren't filld in after this expand.grid

# prep data
clients_all_years <- df.plot %>%
  filter(!is.na(sales), has_sales_zero == TRUE, year_n > = 0) %>%
  # filter(years_of_sales_data == 5, year_n > 0, year_n < 6, !is.na(sales), has_sales_zero = TRUE) %>%
  arrange(RC.Account.Number, year_n) %>%
  group_by(RC.Account.Number) %>%
  mutate(sales_nom_increase_cl = sum(sales, na.rm = TRUE) - sum(lag(sales, order_by = Year), na.rm = TRUE),
         active_year = replace(active_year, is.na(active_year), FALSE),
         sales_cum_sum = cumsum(sales),
         active_before = cumsum(active_year),
         lag_amount = Year - year_zero,
         sales_nom_increase_since_start_cl = sales - sales_in_year_zero,
         # growth_cat = as.factor(( sales_nom_increase_since_start_cl / sales ) > 0.30)
         growth_cat =  cut(sales_nom_increase_since_start_cl / sales, breaks= c(-Inf, 0.30, Inf)),
         growth_cat_year = paste(growth_cat, year_n, sep = "_"),
         growth_cat_year = addNA(growth_cat),
         growth_cat_year = factor(growth_cat_year,
                                  levels = c(levels(growth_cat_year), 'Active - No Data', 'Not renewed'),
                                  labels = c('Grew more than 30%', 'Grew less than 30%', 'Active - No Data', 'Not renewed')),
         active_year = replace(active_year, is.na(active_year), FALSE),
         growth_cat_year = replace(growth_cat_year, active_year == TRUE & is.na(sales), 'Active - No Data'),
         growth_cat_year = replace(growth_cat_year, active_before > 0 & active_year == FALSE, 'Not renewed'),
         vintage_cat = paste(year_one, growth_cat_year, sep = "_"),
         vintage_cat = replace(vintage_cat, active_year == TRUE & is.na(sales), 'Active - No Data'),
         vintage_cat = replace(vintage_cat, active_before > 0 & active_year == FALSE, 'Not renewed'),
         growth_rate_since_y0 = sales / sales_in_year_zero - 1
         # shrank
         # growth_cat_year = ifelse(
         #   active_year == 1 & is.na(growth_cat), NA, growth_cat_year)
  ) %>%
  ungroup() 

clients_sum_all <- clients_all_years %>% 
  filter(active_before > 0, active_year == TRUE, loan_size_cat == '50k-500k')  %>%
  group_by( year_n, growth_cat_year ) %>%    # , year_zero year_one,
  summarise(
    n = n(), 
    nsales = sum(sales > 0, na.rm = TRUE),
    sales_sum = sum(sales, na.rm = TRUE),
    sales_nom_increase = sum(sales - lag(sales, 1), na.rm = TRUE),
    sales_nom_increase_since_start = sum(sales_nom_increase_since_start_cl, na.rm = TRUE)
    # sum(sales_nom_increase_since_start_cl, na.rm = TRUE) - 
    # sum(lag(sales_nom_increase_since_start_cl, 4), na.rm = TRUE),
  )

dim(clients_sum_all)
clients_sum_all

# clients %>% group_by(Year, is.na(sales)) %>% filter(active_year==TRUE) %>% summarise(n())
# clients_sum_all$year_nx <- as.Date(clients_sum_all$year_n, '%Y')


stream.simple.sales <- clients_sum_all %>%
  # filter(year_n<5) %>%
  streamgraph("growth_cat_year", "n", "year_n", scale="continuous") %>% # , interpolate="cardinal"
  # sg_axis_x(1, "year", "%Y") %>%
  # sg_axis_x(1, "year_n", "%Y") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Business Success")
stream.simple.sales


clients5 <- filter(clients, has_sales_zero == 1, 
                   years_of_sales_data >2, years_of_sales_data < 6, year_n >= 0, year_n <=5,
                   loan_size_cat == '50k-500k')

clients5.all.line <- ggplot(clients5, aes(x = year_n, y = sales/1e6, group = RC.Account.Number)) + 
  geom_line() + scale_y_continuous('Sales, millions', labels=scales::dollar)

clients5_sum <- clients5 %>%
  group_by(year_n) %>%
  summarise(
    sales_median = median(sales, na.rm = TRUE),
    growth_median = median(sales_growth_yoy, na.rm = TRUE),
    n = n(),
    n_na = sum(is.na(sales_growth_yoy))
    )

clients5_1 <- distinct(clients5, RC.Account.Number, .keep_all = TRUE)

clients5_1$sales_size_start <- cut(clients5_1$sales_in_year_zero, quantile(clients5_1$sales_in_year_zero))

clients_sales_start_sum <- clients5_1 %>%
  group_by(sales_size_start, year_n, rc_first) %>%
  summarise(sales_growth_median = median(sales_growth), cagr_median = median(sales_cagr), n = n())

# need to filter the one NA..
growth.rcfirst.plot <- ggplot(clients_sales_start_sum[1:12,], aes(x = sales_size_start, y = sales_growth_median, fill = rc_first)) +
  geom_bar(stat = "identity", position = 'dodge') +
  coord_flip()


setwd("C:/Box Sync/jlittel/comms/client financials/data")
save.image(file = 'clientfin4.RData')
  