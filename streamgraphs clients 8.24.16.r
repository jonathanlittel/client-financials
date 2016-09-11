clients_c <- distinct(clients, RC.Account.Number, Year, .keep_all = TRUE)
clients_c_growth <- clients_c %>% 
  mutate(high_growth = sales_growth_yoy > 0.15) %>%
  group_by(high_growth, year_n) %>% 
  filter(year_n > -2, year_n < 10, loan_size_cat == '50k-500k') %>%
  summarise(growth_median = median(sales_growth_yoy, na.rm = TRUE),
            sales_median = median(sales, na.rm = TRUE),
            n = n())

client.growth.plot <- ggplot(clients_c_growth, aes(x = year_n, y = sales_median, color = high_growth)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('sales median', labels = scales::dollar)
client.growth.plot






clients_c <- distinct(clients, RC.Account.Number, Year, .keep_all = TRUE)
clients_c_rc_first <- clients_c %>% 
  group_by(rc_first, year_n) %>% 
  filter(year_n > -2, year_n < 10, loan_size_cat == '50k-500k') %>%
  summarise(growth_median = median(sales_growth_yoy, na.rm = TRUE),
            sales_median = median(sales, na.rm = TRUE),
            n = n())

client.rc.first.plot <- ggplot(clients_c_rc_first, aes(x = year_n, y = sales_median, color = rc_first)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('sales median', labels = scales::dollar)
client.rc.first.plot



clients_c <- distinct(clients, RC.Account.Number, Year, .keep_all = TRUE)
clients_c_afr <- clients_c %>% 
  group_by(rc_first, year_n) %>% 
  filter(year_n > -2, year_n < 10, loan_size_cat == '50k-500k', loan_count == 3, Lending.Region %in% c('WAF', 'EAF')) %>%
  summarise(growth_median = median(sales_growth_yoy, na.rm = TRUE),
            sales_median = median(sales, na.rm = TRUE),
            n = n(),
            n_missing = sum(is.na(sales)))

client.afr.plot <- ggplot(clients_c_afr, aes(x = year_n, y = sales_median, color = rc_first)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('sales median', labels = scales::dollar)
client.afr.plot



save.image(file = 'clientfin3.RData')
getwd()

write.csv(clients, 'clients.csv')
file.show('clients.csv')


#-------------- mikes idea


# create grid with all clients for years 1996 - 2016
clients_years_grid <- expand.grid(Year = seq(1996, 2016, by = 1), RC.Account.Number = unique(clients$RC.Account.Number))
df.plot <- left_join(clients_years_grid, clients, by = c('RC.Account.Number', 'Year'))

# note that a lot of vars aren't filld in after this expand.grid

# prep data
clients_all_years <- df.plot %>%
  arrange(RC.Account.Number, Year) %>%
  group_by(RC.Account.Number) %>%
  mutate(sales_nom_increase_cl = sum(sales, na.rm = TRUE) - sum(lag(sales, order_by = Year), na.rm = TRUE),
         active_year = replace(active_year, is.na(active_year), FALSE),
         sales_cum_sum = cumsum(sales),
         active_before = cumsum(active_year),
         lag_amount = Year - year_zero,
         sales_nom_increase_since_start_cl = sales - sales_in_year_zero,
         # growth_cat = as.factor(( sales_nom_increase_since_start_cl / sales ) > 0.30)
         growth_cat =  cut(sales_nom_increase_since_start_cl / sales, breaks= c(-Inf, 0.30, Inf)),
         growth_cat_year = paste(growth_cat, year_zero, sep = "_"),
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

clients_sum_all<- clients_all_years %>% 
  filter(active_before > 0, active_year == TRUE, loan_size_cat == '50k-500k')  %>%
  group_by( Year, growth_cat_year ) %>%    # , year_zero year_one,
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
clients %>% group_by(Year, is.na(sales)) %>% filter(active_year==TRUE) %>% summarise(n())

stream.simple <- clients_sum_all %>%
  filter(growth_cat_year != 'null', Year < 2016) %>%
  streamgraph("growth_cat_year", "n", "Year", interpolate="cardinal") %>%
  sg_axis_x(1, "year", "%Y") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Business Success")
stream.simple

#-------------- mikes idea complete years

# 
# # prep data
# clients_sum_all <- df.plot %>%
#   arrange(RC.Account.Number, Year) %>%
#   group_by(RC.Account.Number) %>%
#   mutate(sales_nom_increase_cl = sum(sales, na.rm = TRUE) - sum(lag(sales, order_by = Year), na.rm = TRUE),
#          active_year = replace(active_year, is.na(active_year), FALSE),
#          sales_cum_sum = cumsum(sales),
#          active_before = cumsum(active_year),
#          lag_amount = Year - year_zero,
#          sales_nom_increase_since_start_cl = sales - sales_in_year_zero,
#          # growth_cat = as.factor(( sales_nom_increase_since_start_cl / sales ) > 0.30)
#          growth_cat =  cut(sales_nom_increase_since_start_cl / sales, breaks= c(-Inf, 0.30, Inf)),
#          growth_cat_year = paste(growth_cat, year_zero, sep = "_"),
#          growth_cat_year = addNA(growth_cat),
#          growth_cat_year = factor(growth_cat_year,
#                                   levels = c(levels(growth_cat_year), 'Active - No Data', 'Not renewed'),
#                                   labels = c('Grew more than 30%', 'Grew less than 30%', 'Active - No Data', 'Not renewed')),
#          active_year = replace(active_year, is.na(active_year), FALSE),
#          growth_cat_year = replace(growth_cat_year, active_year == TRUE & is.na(sales), 'Active - No Data'),
#          growth_cat_year = replace(growth_cat_year, active_before > 0 & active_year == FALSE, 'Not renewed'),
#          vintage_cat = paste(year_one, growth_cat_year, sep = "_"),
#          vintage_cat = replace(vintage_cat, active_year == TRUE & is.na(sales), 'Active - No Data'),
#          vintage_cat = replace(vintage_cat, active_before > 0 & active_year == FALSE, 'Not renewed')
#          # shrank
#          # growth_cat_year = ifelse(
#          #   active_year == 1 & is.na(growth_cat), NA, growth_cat_year)
#   ) %>%
  # ungroup()

clients_sum_all <- clients_all_years %>%
  filter(active_before > 0, loan_size_cat == '50k-500k')  %>% # , active_year == TRUE
  group_by( Year, vintage_cat ) %>%    # , year_zero year_one,
  summarise(
    n = n(),
    nsales = sum(sales > 0, na.rm = TRUE),
    sales_sum = sum(sales, na.rm = TRUE),
    sales_nom_increase = sum(sales - lag(sales, 1), na.rm = TRUE),
    sales_nom_increase_since_start = sum(sales_nom_increase_since_start_cl, na.rm = TRUE)
    # sum(sales_nom_increase_since_start_cl, na.rm = TRUE) -
    # sum(lag(sales_nom_increase_since_start_cl, 4), na.rm = TRUE),
  )

# dim(clients_sum_all)
clients %>% group_by(Year, is.na(sales)) %>% filter(active_year==TRUE) %>% summarise(n())

stream.vintage2 <- clients_sum_all %>%
  filter(Year < 2015) %>%
  streamgraph("vintage_cat", "n", "Year", interpolate="cardinal") %>%
  sg_axis_x(1, "year", "%Y") %>%
  sg_fill_brewer("PuOr") %>%  # "Spectral" # 
  # sg_fill_tableau("purplegray12") %>%
  sg_legend(show=TRUE, label="Vintage Year")
  # sg_title('Client growth by year of vintage')
stream.vintage2

stream.vintage.2011 <- clients_sum_all %>%
  filter(vintage_cat ==)
  streamgraph("vintage_cat", "n", "Year", interpolate="cardinal") %>%
  sg_axis_x(1, "year", "%Y") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Business Success")
stream.vintage.2011


# completeness <- clients %>%
#   group_by(RC.Account.Number) %>%
#   filter(active_year == TRUE) %>%
#   summarise(active_client_years = sum(active_year, na.rm = TRUE),
#             sales_data_available = as.numeric(sum(!is.na(sales), na.rm = TRUE))) %>%
#   gather(data_type, years, active_client_years:sales_data_available)
# ggplot(completeness, aes(years, group = data_type, color = data_type)) + geom_bar(position = 'dodge')

completeness <- clients %>%
  group_by(RC.Account.Number) %>%
  filter(active_year == TRUE) %>%
  summarise(active_client_years = sum(active_year, na.rm = TRUE),
            sales_data_available = as.numeric(sum(!is.na(sales), na.rm = TRUE)),
            sales_data_avail_pct = sales_data_available / active_client_years) %>%
  group_by(active_client_years) %>%
  summarise(client_years = n(), average_sales_data_available = mean(sales_data_avail_pct, na.rm = TRUE))


compl.plot <- ggplot(completeness, aes(x = active_client_years, y = average_sales_data_available, label = client_years)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous('Average available sales data', labels = scales::percent) +
  geom_text(aes(label=client_years),hjust=0.5, vjust=1)
compl.plot

completeness.sales <- clients %>%
  group_by(RC.Account.Number, year_one) %>%
  filter(active_year == TRUE) %>%
  summarise(active_client_years = sum(active_year, na.rm = TRUE),
            sales_data_available = as.numeric(sum(!is.na(sales), na.rm = TRUE)),
            sales_data_avail_pct = sales_data_available / active_client_years) %>%
  group_by(sales_data_available, year_one) %>%
  summarise(client_years = n())

completeness.sales <- rename(completeness.sales, vintage_year = year_one) %>% mutate(vintage_year = factor(vintage_year))
compl.sales.plot <- ggplot(completeness.sales, aes(x = sales_data_available, y = client_years, fill = vintage_year)) + 
  geom_bar(stat = "identity") + scale_y_continuous('Number of clients') # + scale_fill_brewer(palette = 'YlOrBr')
compl.sales.plot
