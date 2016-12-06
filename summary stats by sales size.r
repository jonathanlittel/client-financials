# summary stats on sales size
lc$date <-  as.Date(lc$Earliest.Disbursement.Date, '%d/%m/%Y')
lc$Date.Repaid <-  as.Date(lc$Date.Repaid, '%d/%m/%Y')
# add sales and pd to bal_today

pd <- select(df.pd, RC.Opp.Number, pd, WriteoffsDummy)
bal_today <- bal_today %>%
  left_join(pd, by = RC.Opp.Number)

# read sems
    filename <- 'Analysis of RAP Impact Metric Database_v5.xlsx'
    sem_raw <- read_excel(filename, sheet = 'Database', skip = 1)
    sem_raw <- sem_raw[,1:111]
	 sem <- sem_raw %>%
      select(RC.Opp.Number = Rcoppnumber, additionality = `Loan Additionality POINT`,
      impact_score = `Final Impact Score`) %>%
      filter(!is.na(RC.Opp.Number))


# add sales in year zero
clients_y_0_sales <- select(clients, RC.Account.Number, sales_in_year_zero, loan_size_cat)
clients_y_0_sales <- clients_y_0_sales %>% 
	group_by(RC.Account.Number) %>%
	fill(sales_in_year_zero, loan_size_cat, .direction = c('down', 'up')) %>%
	ungroup()

clients_y_0_sales <- clients_y_0_sales %>% distinct(RC.Account.Number, .keep_all = TRUE)
# clients_y_0_sales <- clients_y_0_sales[,1:2]

# add active/not active
	active_status <- loans %>%
		select(RC.Opp.Number, active_today) %>%
		distinct(RC.Opp.Number, active_today) %>%
		ungroup()

lc_plus <- lc %>%
  left_join(pd, by = 'RC.Opp.Number') %>%
  mutate(
  	Year = year(date)) %>%
  left_join(clients_y_0_sales, by = c('RC.Account.Number')) %>%
  left_join(active_status[,1:2], by = 'RC.Opp.Number') %>%
  left_join(sem, by = 'RC.Opp.Number')

lc_plus$sales_cat_zero <- cut_width(lc_plus$sales_in_year_zero, 5e5)
lc_plus$sales_cat_zero <- cut(lc_plus$sales_in_year_zero, 
 c(-Inf,2.5e5, 5e5, 1e6, 1.5e6, 2e6, 3e6, 4e6, 5e6, 1e7,2e7,5e7,Inf),
 labels = c('<= 250k', '250k-500k', '500k-1M', '1M-1.5M', '1.5M-2M', '2M-3M',
 '3M-4M', '4M-5M', '5M-10M', '10M-20M', '20M-50M', '50M+'))

loan_stats <- lc_plus %>%
	group_by(sales_cat_zero) %>%
	filter(active_today == FALSE) %>%
	# filter(active_today == FALSE, Year < 2014) %>%
	summarize(
		pd_median = median(pd, na.rm = T),
		pd_mean = mean(pd, na.rm = T), 
		wo_mean = mean(WriteoffsDummy, na.rm = T),
		n = n(),
		n_has_pd = sum(!is.na(pd)),
		additionality_mean = mean(additionality, na.rm = TRUE),
		impact_score_mean = mean(impact_score, na.rm = TRUE)
		)

loan_stats[,c(2:4,7:8)] <- round(loan_stats[,c(2:4,7:8)], 2)
# loan_stats[,2:4] <- paste(loan_stats[,2:4], '%', sep ='')
DT::datatable(loan_stats, caption = 'Completed Loans - risk by sales bins')	

loan_stats_2013_earlier <- lc_plus %>%
	group_by(sales_cat_zero) %>%
	# filter(active_today == FALSE) %>%
	filter(active_today == FALSE, Year < 2014) %>%
	summarize(
		pd_median = median(pd, na.rm = T),
		pd_mean = mean(pd, na.rm = T), 
		wo_mean = mean(WriteoffsDummy, na.rm = T),
		n = n(),
		n_has_pd = sum(!is.na(pd)),
		additionality_mean = mean(additionality, na.rm = TRUE),
		impact_score_mean = mean(impact_score, na.rm = TRUE)
		)
loan_stats_2013_earlier[,c(2:4,7:8)] <- round(loan_stats_2013_earlier[,c(2:4,7:8)], 2)


# add pd and sales in year zero to bal_today

clients_y_0_sales
df.pd_cl <- df.pd %>% group_by(RC.Account.Number) %>%
	summarise(pd = mean(pd, na.rm = T),
		WriteoffsDummy  = max(WriteoffsDummy))

bal_today_plus <- bal_today %>%
  left_join(df.pd_cl, by = 'RC.Account.Number') %>%
  # mutate(
  	# Year = year(date)) %>%
  left_join(clients_y_0_sales, by = c('RC.Account.Number')) 

bal_today_plus$sales_cat_zero <- cut_width(bal_today_plus$sales_in_year_zero, 5e5)
bal_today_plus$sales_cat_zero <- cut(bal_today_plus$sales_in_year_zero, 
 c(-Inf,2.5e5, 5e5, 1e6, 1.5e6, 2e6, 3e6, 4e6, 5e6, 1e7,2e7,5e7,Inf),
 labels = c('<= 250k', '250k-500k', '500k-1M', '1M-1.5M', '1.5M-2M', '2M-3M',
 '3M-4M', '4M-5M', '5M-10M', '10M-20M', '20M-50M', '50M+'))

client_today_stats <- bal_today_plus %>%
	group_by(sales_cat_zero) %>%
	summarize(
		pd_median = median(pd, na.rm = T),
		pd_mean = mean(pd, na.rm = T), 
		wo_mean = mean(WriteoffsDummy, na.rm = T),
		n = n(),
		n_has_pd = sum(!is.na(pd))
		)	

client_today_stats[,c(2:4)] <- round(client_today_stats[,c(2:4)], 2)
