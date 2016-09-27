
# new clients by year
  # only for clients in 50k-500k
  table(filter(clients, Year==year_one, loan_size_cat=='50k-500k')$Year)
  table(filter(clients, Year==year_one)$Year)  
  
  
#-------------- mikes idea years_of_sales_data

# create grid with all clients for years 1996 - 2016
clients_years_grid <- expand.grid(year_n = seq(1, max(clients$year_n, na.rm = T), by = 1), RC.Account.Number = unique(clients$RC.Account.Number))
df.plot <- left_join(clients_years_grid, clients, by = c('RC.Account.Number', 'year_n'))

# note that a lot of vars aren't filld in after this expand.grid

# prep data
clients_all_years <- df.plot %>%
  filter(!is.na(sales), has_sales_zero == TRUE, year_n >= 0) %>%
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
    n_sales_data = sum(!is.na(sales)),
    n_na_yoy_growth = sum(is.na(sales_growth_yoy)),
    producers_median = median(producers, na.rm = TRUE), 
    wages_median = median(wages, na.rm = TRUE),
    payments_median = median(payments_to_producers, na.rm = TRUE)
    )

clients5_1 <- distinct(clients5, RC.Account.Number, .keep_all = TRUE)

clients5_1$sales_size_start <- cut(clients5_1$sales_in_year_zero, quantile(clients5_1$sales_in_year_zero))

clients5_1$sales_size_start <- cut(clients5_1$sales_in_year_zero, c(0, 2.5e5, 5e5, 1e6, Inf), 
                                labels = c('0 to 250k',
                                           '250k to 500k',
                                           '500k to $1m',
                                           '1m+'))

portfolio_model_output <- clients %>%
  filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
  group_by(years_active, year_n, active_today) %>% # or years_active
  summarise(
    balance_mean   = mean(balance),
    producers_mean = mean(producers, na.rm = TRUE),
    payments_to_producers_mean = mean(payments_to_producers, na.rm = TRUE),
    revenue_mean = mean(revenue, na.rm = TRUE),
    # el = mean(el, na.rm = TRUE),
    revenue_less_risk_mean = mean(revenue_less_risk, na.rm = TRUE),
    n = n()
  )

write.csv(portfolio_model_output, 'port_assums.csv', row.names = FALSE)
file.show('port_assums.csv')

# balance growth
options(scipen = 10)
ggplot(portfolio_model_output, aes( x = year_n, y = balance, group = years_active, color = factor(years_active))) + geom_line()

clients_sales_start_sum <- clients5_1 %>%
  group_by(sales_size_start, year_n, rc_first) %>%
  summarise(sales_growth_median = median(sales_growth), cagr_median = median(sales_cagr), n = n())

# need to filter the one NA..
growth.rcfirst.plot <- ggplot(clients_sales_start_sum[1:12,], aes(x = sales_size_start, y = sales_growth_median, fill = rc_first)) +
  geom_bar(stat = "identity", position = 'dodge') + scale_y_continuous('Median Sales Growth', labels = scales::percent) +
  coord_flip()


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

#--------------  payments growth ---------------------

ggplot(clients, aes(x = year_n, y = Purchases.from.producers)) + geom_line(aes(group = RC.Account.Number)) + geom_smooth()


pmts.plot <- ggplot(
  filter(
    clients, year_n > -1, year_n < 11, !is.na(Lending.Region), years_of_sales_data > 3,
    Lending.Region %in% c('WAF', 'EAF', 'MAC', 'SAM'), loan_size_cat == '50k-500k',
    active_year == TRUE), 
  aes(x = year_n, y = payments_to_producers, group = RC.Account.Number)) + 
  geom_line(alpha = 0.25, aes(color = "Client", size = "Client")) + 
  geom_line(stat = "smooth", method = "loess", aes(group = Lending.Region, color = "Lending Region", size = "Lending Region"), alpha = 0.5) +
  facet_wrap( ~ Lending.Region, nrow = 2) + 
  scale_y_log10('Payments to Producers ($M)', labels = scales::dollar, breaks = c(1e5, 1e6, 2.5e6, 5e6, 1e7)) +  # 
  scale_x_continuous(breaks = c(0:10)) +
  scale_color_manual(name = "Unit", values = c("Client" = "black", "Lending Region" = "dodgerblue1")) +
  scale_size_manual(name = "Unit", values = c("Client" = 0.75, "Lending Region" = 2.5))
pmts.plot

clients %>% filter(active_year==1, loan_size_cat == '50k-500k') %>% 
  group_by(year_n) %>% summarize(mean(payments_to_producers, na.rm = TRUE), median(payments_to_producers, na.rm = TRUE))

clients %>% filter(active_year==1, loan_size_cat == '50k-500k') %>% group_by(year_n) %>% 
  summarize(mean(payments_growth, na.rm = TRUE), median(payments_growth, na.rm = TRUE), n = sum(payments_growth>0, na.rm = TRUE))

prod.plot <- ggplot(
  filter(
    clients, year_n > -1, year_n < 11, !is.na(Lending.Region), years_of_sales_data > 3,
    Lending.Region %in% c('WAF', 'EAF', 'MAC', 'SAM')), 
  aes(x = year_n, y = producers, group = RC.Account.Number)) + 
  geom_line(alpha = 0.25, aes(color = "Client", size = "Client")) + 
  geom_line(stat = "smooth", method = "loess", aes(group = Lending.Region, color = "Lending Region", size = "Lending Region"), alpha = 0.5) +
  facet_wrap( ~ Lending.Region, nrow = 2) + 
  scale_y_log10('Producers', labels = scales::comma, breaks = c(1e2, 1e3, 1e4, 1e5)) + 
  scale_x_continuous(breaks = c(0:10)) +
  scale_color_manual(name = "Unit", values = c("Client" = "black", "Lending Region" = "dodgerblue1")) +
  scale_size_manual(name = "Unit", values = c("Client" = 0.75, "Lending Region" = 2.5))
prod.plot


prod.lm <- lm(producers ~ year_n + lag(producers,1) + active_year, data = filter(clients, year_n < 7, producers > 0))
summary(prod.lm)
clients %>% filter(active_year==1, loan_size_cat == '50k-500k') %>% 
  group_by(year_n) %>% summarize(mean(producers, na.rm = TRUE), median(producers, na.rm = TRUE))

clients %>% filter(active_year==1, loan_size_cat == '50k-500k') %>% group_by(year_n) %>% 
  summarize(mean(producers_growth, na.rm = TRUE), median(producers_growth, na.rm = TRUE))

wages.ag.plot <- ggplot(
  filter(
    clients, year_n > -1, year_n < 11, !is.na(Lending.Region), years_of_sales_data > 3,
    loan_size_cat == '50k-500k',
    processing_type != 'NA - Need to dig into memos',
    !is.na(processing_type),
    active_year == TRUE), 
  aes(x = year_n, y = wages, group = RC.Account.Number)) + 
  geom_line(alpha = 0.25, aes(color = "Client", size = "Client")) + 
  geom_line(stat = "smooth", method = "loess", aes(group = processing_type, color = "Processing Type", size = "Processing Type"), alpha = 0.5) +
  facet_wrap( ~ processing_type, nrow = 2) + 
  scale_y_log10('Wages', labels = scales::dollar, breaks = c(1e5, 1e6, 2.5e6, 5e6, 1e7)) +  # 
  scale_x_continuous(breaks = c(0:10)) +
  scale_color_manual(name = "Unit", values = c("Client" = "black", "Processing Type" = "dodgerblue1")) +
  scale_size_manual(name = "Unit", values = c("Client" = 0.75, "Processing Type" = 2.5))
wages.ag.plot


#-------------- revenue --------------------

ggplot(
  filter(
    clients, year_n > -1, year_n < 8, !is.na(Lending.Region), years_of_sales_data > 3,
    loan_size_cat == '50k-500k',
    active_year == TRUE), 
  aes(x = year_n, y = revenue_less_risk, group = RC.Account.Number)
) + geom_line() +geom_smooth(aes(group = Lending.Region, color = Lending.Region)) + geom_smooth()


net.rev.plot <- ggplot(
  filter(
    clients, year_n > -1, year_n < 8, !is.na(Lending.Region), years_of_sales_data > 3,
    loan_size_cat == '50k-500k',
    active_year == TRUE,
    revenue_less_risk < 200000,
    revenue_less_risk > -100000), 
  aes(x = year_n, y = revenue_less_risk, group = RC.Account.Number)) +
  geom_boxplot(aes(group = year_n, color = year_n), alpha = 0.5) + geom_jitter(alpha = 0.2, shape = 43) +
  scale_y_continuous(labels = scales::dollar)
net.rev.plot

high_growth_client_names <- clients %>% filter(sales_growth > 5, !is.na(Account.Name)) %>% 
  select(Account.Name, RC.Account.Number, sales_growth, sales_cagr, sales_in_year_zero, year_one, active_today) %>% distinct(.keep_all = TRUE)

high_growth_client_names <- rbind( high_growth_client_names,
  clients %>% filter(sales_cagr > 0.3, !is.na(Account.Name)) %>% 
  select(Account.Name, RC.Account.Number, sales_growth, sales_cagr, sales_in_year_zero, year_one, active_today) %>% distinct(.keep_all = TRUE)
) %>% distinct(RC.Account.Number, .keep_all = TRUE)

write.csv(high_growth_client_names, 'high_growth_clients.csv', row.names = FALSE)

#--------------- sales growth by animal ------------------

sales.animal.plot <- ggplot(
  filter(
    cl, year_n > -1, year_n < 8, !is.na(Lending.Region), # years_of_sales_data > 3,
    loan_size_cat == '50k-500k',
    active_year == TRUE),
  aes( x = year_n, y = sales)) +
    geom_line(aes(group = growth_animal, color = growth_animal))
sales.animal.plot
  
p <- ggplot(filter(clients, Year==year_one, loan_size_cat=='50k-500k'), aes(x = Year)) +
  # ggplot(filter(clients, Year==year_one), aes(x = Year, group = loan_size_cat)) +
  # geom_histogram(binwidth = 1, alpha = 0.5) +
  geom_histogram(bins = 18, alpha = .35) +
  stat_bin(aes(y=..count.., label=..count..), binwidth = 1, geom="text", vjust=-.5, bins = 18) +
  theme_minimal()
p

ggplot(filter(clients, year_n > -4, year_n < 14), aes(sales, group = year_n, color = year_n>0)) + geom_density()
ggplot(filter(clients, year_n > -4, year_n < 14), aes(log(sales), group = year_n, color = year_n)) + geom_density()
ggplot(filter(clients, year_n > -4, year_n < 14), aes(log(sales), group = years_active, color = years_active)) + geom_density()
ggplot(filter(clients, year_n > -4, year_n < 14), aes(log(sales), group = Year==year_one)) + geom_density()


setwd("C:/Box Sync/jlittel/comms/client financials/data")
save.image(file = 'clientfin4.RData')
  