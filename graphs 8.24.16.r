loans_c <- distinct(loans, RC.Opp.Number, loan_number, .keep_all = TRUE)
loans_c_sector <- loans_c %>% 
  filter(active_today != 1, loan_size_cat == '50k-500k') %>%
  group_by(Sector.and.Perishability, loan_number) %>% 
  summarise(revenue_per_client = sum(revenue_less_risk, na.rm = TRUE)/n(), n = n())

loan.n.rev.plot <- ggplot(loans_c_sector, aes(x = loan_number, y = revenue_per_client, color = Sector.and.Perishability)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('Revenue per client (predict', labels = scales::dollar)
loan.n.rev.plot

loan.n.rev.plot5 <- ggplot(filter(loans_c_sector, loan_number<5), aes(x = loan_number, y = revenue_per_client, color = Sector.and.Perishability)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('Revenue per client less expected loss', labels = scales::dollar)
loan.n.rev.plot5


loans_c_3 <- loans_c %>% 
  filter(active_today != 1, loan_size_cat == '50k-500k', loan_count == 3, Sector.and.Perishability != 'High Perishable Ag', Sector.and.Perishability != 'High Perishable Ag') %>%
  group_by(Sector.and.Perishability, loan_number) %>% 
  summarise(revenue_per_client = sum(revenue_less_risk, na.rm = TRUE)/n(), n = n())

loan.n.rev.plot3 <- ggplot(filter(loans_c_3), aes(x = loan_number, y = revenue_per_client, color = Sector.and.Perishability)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('Revenue per client (predict', labels = scales::dollar)
loan.n.rev.plot3

# look at clients that started with $250k - 750k sales
loans2 <- loans %>%
  group_by(RC.Account.Number) %>%
  arrange(Year) %>%
  fill(sales, .direction = c('up')) %>%  
  fill(processing_type, .direction = c('down', 'up')) %>%  
  filter( loan_size_cat == '50k-500k') %>%
  ungroup() 


#   loans2   %>%
#     filter(Year == year_zero) %>%
#     mutate(sales_year_zero = sales) %>%
#     select(RC.Account.Number, sales_year_zero) %>%
#     right_join(loans, by = 'RC.Account.Number')
#   
#   loans_small_sales <- loans %>%
#     filter(active_today != 1, loan_size_cat == '50k-500k') %>%
#     mutate(sales_cat = sales_year_zero < 5e5) %>%
#     group_by(year_of_loan, sales_cat) %>%
#     summarise(sales_median = median(sales, na.rm = TRUE), n = n())
#   
# small.sales.plot <- ggplot(filter(loans_small_sales), aes(x = year_of_loan, y = sales_median) ) + geom_line()
# small.sales.plot


#-------clients that started with small growth --------------
clients <- clients %>%
  filter(Year == year_one) %>%
  mutate(high_growth = sales_growth_yoy > 0.15) %>%
  select(RC.Account.Number, high_growth) %>%
  right_join(clients)

high_growth <- clients %>%
  filter(loan_count == 3) %>%
  group_by(high_growth, year_of_loan) %>%
  summarise(sales_median = median(sales, na.rm = TRUE), n = n())

high.growth.plot <- ggplot(filter(high_growth, year_of_loan<5), aes(x = year_of_loan, y = sales_median, color = high_growth)) + geom_line()
high.growth.plot

#------- region  --------------
loans_c <- distinct(loans, RC.Opp.Number, loan_number, .keep_all = TRUE)
loans_c_region <- loans_c %>% 
  filter(active_today != 1, loan_size_cat == '50k-500k') %>%
  group_by(Lending.Region, loan_number) %>% 
  summarise(revenue_per_client = sum(revenue_less_risk, na.rm = TRUE)/n(), n = n())

loan.region.rev.plot <- ggplot(loans_c_region, aes(x = loan_number, y = revenue_per_client, color = Lending.Region)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('Revenue per client less expected loss', labels = scales::dollar)
loan.region.rev.plot


#------- region sales --------------
loans_c <- distinct(loans, RC.Opp.Number, loan_number, .keep_all = TRUE)
loans_c_region <- loans_c %>% 
  filter(active_today != 1, loan_size_cat == '50k-500k') %>%
  group_by(Lending.Region, loan_number) %>% 
  summarise(sales_median = median(sales, na.rm = TRUE), n = n())

loan.region.sales.plot <- ggplot(loans_c_region, aes(x = loan_number, y = sales_median, color = Lending.Region)) + 
  geom_line() +
  geom_text(aes(label = n), nudge_y = 3, size = 3) +
  scale_y_continuous('Median sales', labels = scales::dollar)
loan.region.sales.plot



  