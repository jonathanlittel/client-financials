# note: requires source('txns from sf.r') first, to load tx2, which is transaction and balance reports from SF

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readxl)
library(readr)

# --------------------------------------------------------
# LOAD AND PREPARE DATA

# Read historical balance data
# add account number to below report, export details as csv
# https://rootcapital.my.salesforce.com/00O700000054Uu2
  wd_loc <- 'c:/Box Sync/jlittel/comms/client financials/data'
  setwd(wd_loc)
  # bal <- read.csv('SF_balances_by_date_08.22.16.csv')
  # bal <- select(bal, LoanID=Loan.ID, RC.Account.Number=RC.Account.Number, Account.Name=Account.Name,
  #               RC.Opp.Number = RC.Opp.Number,
  #               balance=Internal.Outstanding.Principal, date=Date)
  # 
  bal <- read_csv('SF_balances_by_date_09.22.16.csv')
  bal <- select(bal, LoanID=`Loan ID`, RC.Account.Number=`RC Account Number`, Account.Name=`Account Name`,
                RC.Opp.Number = `RC Opp Number`,
                balance=`Internal Outstanding Principal`, date=Date)
  
  bal$date <- as.Date(as.character(bal$date), format='%m/%d/%Y')
  bal$Year <- as.numeric(year(bal$date))

# Determine if active during Year
  bal <- bal %>%
    group_by(RC.Account.Number, date) %>%
    mutate(bal_sum = sum(balance, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(RC.Account.Number, Year) %>%
    mutate(
      bal_avg_cl = mean(bal_sum, na.rm = TRUE),
      bal_peak_cl = max(bal_sum, na.rm = TRUE), 
      active_year = bal_peak_cl > 0
      ) %>%
    ungroup() %>%
    group_by(RC.Opp.Number) %>%
    mutate(bal_loan_peak = max(balance, na.rm = TRUE)) %>%
    ungroup()

  bal$bal <- NULL

# find peak balance of loan
  bal <- bal %>%
    group_by(LoanID) %>%
    mutate(
      months_outstanding = sum(bal_sum > 0),
      active_today = ifelse(date == '2016-08-31' & bal_sum > 0, 1, 0),      
      bal_peak_loan = max(balance, na.rm = TRUE),
      bal_avg_loan = mean(balance, na.rm = TRUE)
      ) %>%
    ungroup()

# variable for cumulative count and the (order) number of a loan
  # will need to be merged on account, LoanID (not year)
  bal2 <- bal %>%
    group_by(LoanID) %>%
    mutate(date_min = min(date)) %>%
    distinct(LoanID, .keep_all = TRUE) %>%
    ungroup() %>%    
    group_by(RC.Account.Number) %>%
    arrange(date_min) %>%
    mutate(
      loan_count = n(),
      loan_number = row_number(),
      Year = year(date_min)
    ) %>%
    select(RC.Account.Number, LoanID, loan_count, loan_number)  %>% # Year
    inner_join(bal, by = c('LoanID', 'RC.Account.Number'))  # loan level info
      
# variables for client's first loan-year
  bal3 <- bal2 %>%
    group_by(RC.Account.Number) %>%
    # filter(Year == min(Year)) %>%
    filter(date == min(date)) %>%
    ungroup() %>%
    distinct(Year, RC.Opp.Number, .keep_all = TRUE) %>%
    group_by(RC.Account.Number) %>%   
    mutate( balance_first = bal_loan_peak,    # bal_loan_peak
    	loan_size_cat = ifelse(balance_first >= 5e4 & balance_first <= 5e5, '50k-500k',
    		ifelse(balance_first < 5e4, '<50k',
    			ifelse(balance_first > 5e5, '>500k', NA)))
    	) %>%
    mutate(
    	loan_size_cat_scott = ifelse(balance_first >= 5e4 & balance_first <= 2e5, '50k-200k',
    		ifelse(balance_first < 5e4, '<50k',
    			ifelse(balance_first > 2e5, '>200k', NA)))
    	) %>%
    mutate(
      loan_size_cat = factor(loan_size_cat),
      loan_size_cat_scott = factor(loan_size_cat_scott)
    ) %>%
    distinct(Year, RC.Account.Number, .keep_all = TRUE) %>%
    select(RC.Account.Number, balance_first, loan_size_cat, loan_size_cat_scott) %>%
    right_join(bal2, by = 'RC.Account.Number') %>%
    ungroup()

table(bal3$loan_size_cat)
dim(bal3) == dim(bal2)      # should be TRUE FALSE
dim(bal3)
# filter to one row per RC.Opp.Number and Year
  bal4 <- distinct(bal3, RC.Opp.Number, Year, .keep_all = TRUE) %>%
    select(-date)
  sum(duplicated(bal4[,c('RC.Opp.Number', 'Year')])) 
  
# Replace NAs function
  replace_na <- function(x, replacement = 0) {
    # if(class(x) != 'numeric') stop('item must be numeric')
    y <- x
    y[is.na(x)] <- replacement
    y
  }

# read transactions from salesforce
  # source('txns from sf.r') # run this when need to refresh sf data
  tx2 <- read.csv('tx2.csv') 
# load more SEM data
    # sems_raw <- read.csv('master_data_with_alex.csv')
    sems_raw <- read_excel('SEMs for all Clients.xlsx', sheet = 1, na = "NA")
    sems <- select(sems_raw,
    	RC.Account.Number = `RC Account Number`,
    	Year = `Data_Year`,
    	sales_sem = `Revenue from Sales`,
    	rc_first = `Root Capital first lender?`,
    	producers = Producers,
    	payments_to_producers = `Payments to Producers`,
    	hectares = Hectares,
    	wages = `Wages and Salaries Paid to Employees`,
    	processing_type = `Processing?`
    	)
    # sems <- select(sems_raw, 
    #                RC.Account.Number = `RC.Account.Number`,
    #                Year = `Data_Year`,
    #                sales_sem = `Revenue.from.Sales`,
    #                rc_first = `Root.Capital.first.lender.`,
    #                producers = Producers,
    #                payments_to_producers = `Payments.to.Producers`,
    #                hectares = Hectares,
    #                wages = `Wages.and.Salaries.Paid.to.Employees`,
    #                processing_type = `Processing.`
    # )
    # remove ~192 rows that only have an account name, not account number
    ind <- is.na(sems$RC.Account.Number)
    # data is loan level, filter to client level (presuming duplicate loan-years are same data)
    sems <- sems[!ind,]
    sems <- sems %>% distinct(RC.Account.Number, Year, .keep_all = TRUE)
    
# read client financials data  
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  filename <-  "rap_data_Q4_15_05.24.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  client_fin <- select(df.rap, RC.Opp.Number, RC.Account.Number, Year, Sales, Purchases.from.producers, Total.COGS, Total.Income)
  wd <- 'c:/Box Sync/jlittel/comms/client financials/'
  # setwd(wd)
    pd_loc <- 'https://rootcapital.box.com/shared/static/jpklpygf1n0se6axtjljz978w41mm3uv.csv'
    pds <- read.csv(pd_loc)
    pds <- select(pds, RC.Opp.Number, pd)
    
# read loan characteristics
		# careful, this is loan level and fin is client level
	 	lc_filename <- 'https://rootcapital.box.com/shared/static/gme71utefwvj48i3t3jia5iaqdgplh2f.csv'
	 	lc <- read.csv(lc_filename)
	 	# names(lc)[8] <- 'RC.Account.Number'
	 	names(lc)[5] <- 'LoanID'
	 	client_char <- filter(lc, !duplicated(RC.Account.Number))
	 	client_char <- select(client_char, RC.Account.Number, Lending.Region,
	 		Portfolio, Sector.and.Perishability, Internal.Interest.Rate...., Loan.Tenor, Loan.Type, Loan.Use)
	 	id_match <- lc %>% select(RC.Account.Number, Account.Name) %>% distinct(RC.Account.Number, Account.Name)
# read writeoffs
	 	wo_file <- 'https://rootcapital.box.com/shared/static/om5mw9gj418w2jich40hasbb09fvjtih.csv'
	 	wo <- read.csv(wo_file)
    wo <- distinct(wo, LoanID, .keep_all = TRUE)
# load sf data (produces object 'tx' - loan level on RC Opp Number)
	 	# source('C:/Box Sync/jlittel/comms/client financials/txns from sf.r')
	 	ids <- select(lc, RC.Opp.Number, RC.Account.Number, Account.Name)
	 	tx3 <- left_join(tx2, ids, by = 'RC.Opp.Number')
	 	names(lc)
	 	# add RC.Account.Number to txn data for merging with balance data
# read sems
    setwd(wd_loc)
    filename <- 'Analysis of RAP Impact Metric Database_v5.xlsx'
    sem_raw <- read_excel(filename, sheet = 'Database', skip = 1)
    sem_raw <- sem_raw[,1:111]
	 	sem <- sem_raw %>%
      select(RC.Opp.Number = Rcoppnumber, additionality = `Loan Additionality POINT`,
      impact_score = `Final Impact Score`) %>%
      filter(!is.na(RC.Opp.Number))
# --------------------------------------------------------
# MERGE    

	 	m1a <- full_join(x = bal4, y = tx3, by = c('RC.Opp.Number', 'Account.Name', 'RC.Account.Number')) # Account.Name converted to char
	 	setdiff(bal3$RC.Opp.Number, tx2$RC.Opp.Number)  # check that everything will match
	 	setdiff(tx2$RC.Opp.Number, bal3$RC.Opp.Number)  # check that everything will match
	 	m2a <- m1a
	 	m3a <- left_join(m2a, pds, by = 'RC.Opp.Number')
    m4a <- left_join(m3a, client_char, by = 'RC.Account.Number')
    m5a <- left_join(m4a, wo, by = 'LoanID')
    m6a <- left_join(m5a, client_fin, by = c('RC.Opp.Number', 'RC.Account.Number', 'Year'))
    m7a <- full_join(m6a, sems, by = c('RC.Account.Number', 'Year'))
    dim(m5a) - dim(m6a)
    dim(m7a)
    m8a <- left_join(m7a, sem, by = 'RC.Opp.Number')
    loans <- m8a

    # check for duplicate rows
    sum(duplicated(loans[,c('RC.Opp.Number', 'Year')])) -
    sum(is.na(loans$RC.Opp.Number)) - sum(is.na(loans$Year)) # need to account for spots where both are na
    
    
# --------------------------------------------------------
# NEW VARIABLES

	loans$balance[is.na(loans$balance)] <- 0
  loans$WriteoffsDummy <- replace_na(loans$WriteoffsDummy, 0)

# consolidate sem and FS data sources
  loans$sales <- ifelse(is.na(loans$Sales), loans$sales_sem, loans$Sales)
  loans$Sales <- NULL
  loans$sales_sem <- NULL
  loans$payments_to_producers <- ifelse(is.na(loans$payments_to_producers), 
                  loans$Purchases.from.producers, loans$payments_to_producers)
  loans$Purchases.from.producers <- NULL
  

# find year of first balance and years active
 	loans <- loans %>%
 			group_by(RC.Account.Number) %>%
 			filter(active_year==TRUE) %>%
 			mutate(
 				year_one = min(Year),
 				year_last = max(Year),
 				year_zero = year_one - 1,
 				years_active = year_last - year_zero			
 				# year_of_loan = Year - year_zero
 				) %>%
 	    select(RC.Account.Number, year_one:years_active) %>%
 	  distinct(RC.Account.Number, .keep_all = TRUE) %>%
 	  full_join(loans, by = c('RC.Account.Number'))

# find loan-year yoy sales growth
 	loans <- loans %>%
 	  group_by(RC.Opp.Number) %>%
 	  arrange(Year) %>%
 	  mutate(sales_growth_yoy = sales / ( lag(sales) - 1)) %>%  # but really should do this again at client
 	                                                            # some Opps won't have previous year (and are different #s..)
 	  ungroup()
 	
 # fill in sems and client level characteristics
loans <- loans %>%
  group_by(RC.Account.Number) %>%
  fill(processing_type, rc_first, producers, payments_to_producers, 
       loan_size_cat, loan_size_cat_scott, .direction = c('down', 'up')) 

  # remove duplicate rows... 110317 had two rows for 2005 at same loan id.. (?)
    # different sales #s (had two LoanIDs with different sales #s for that year)
  
  
  source('C:/Box Sync/jlittel/comms/client financials/simple pd model.r')

# prep revenue to get one rev/el/net per loan
rev_temp <- loans %>%
  # filter(Year == year_one) %>%   # to avoid counting for multiple years - but leaves only year_n...
  group_by(RC.Opp.Number) %>%
  filter(Year == min(Year)) %>%
  distinct(RC.Opp.Number, .keep_all = TRUE) %>% # this should be redundant with above line...
  ungroup() %>%
  group_by(RC.Account.Number, Year) %>%
  summarise(
    revenue_          = sum(revenue_, na.rm=TRUE),
    el                = sum(el, na.rm=TRUE),
    revenue_less_risk_check = sum(revenue_less_risk, na.rm=TRUE),
    revenue_less_risk = sum(revenue_ - el, na.rm=TRUE),
    revenue_less_risk_per_year = sum(revenue_less_risk_per_year, na.rm=TRUE),
    revenue_less_risk_less_debt = sum(revenue_less_risk_less_debt, na.rm = TRUE),
    revenue_less_risk_less_debt_per_year = sum(revenue_less_risk_less_debt_per_year, na.rm = TRUE)
    ) %>%
  select(RC.Account.Number, Year, revenue_less_risk, revenue_less_risk_per_year, 
    revenue_less_risk_less_debt, revenue_less_risk_less_debt_per_year,
    revenue_est = revenue_, expected_loss = el ) %>%
  distinct(RC.Account.Number, Year, .keep_all = TRUE)


#--------- PREP OUTPUT -------------------------------------------
  
	 # remove duplicates of Year/Loan and fill in the blanks for RC.Account.Number
	 # note that revenue etc won't work
	   clients <- loans %>%
	      group_by(RC.Account.Number) %>%
	      fill( rc_first, Account.Name,
	           loan_size_cat, loan_size_cat_scott, .direction = c("down", "up")) %>%
	     group_by(RC.Account.Number, Year) %>%
	     fill(sales, producers, hectares, payments_to_producers, wages, 
	          .direction = c("down", "up")) %>%	     
	      distinct(Year, RC.Account.Number, .keep_all = TRUE) %>%
	      select(RC.Account.Number:active_today, Lending.Region, rc_first, producers, payments_to_producers, wages,
	             sector = Sector.and.Perishability, balance_first, loan_size_cat, loan_size_cat_scott,
	             revenue:Internal.Interest.Rate...., processing_type, sales, # sales_growth_yoy,
	             active_year,
	             -LoanID) %>% # careful about going from loan to client
	      select(-RC.Opp.Number) %>%
	      select(-balance) %>%     # this is just one per client, from bal4
	      select(-active_year) %>%     # this is just one per client, from bal4
	     ungroup()
	   
	   clients <- left_join(clients, rev_temp, by = c('RC.Account.Number', 'Year'))

     clients$bal_avg_cl <- NULL
	   clients$bal_peak_cl <- NULL

	   clients <- bal2 %>%
	     group_by(RC.Account.Number, Year) %>%
  	   summarise(
        bal_avg  = max(bal_avg_cl, na.rm = TRUE),
        bal_peak = max(bal_peak_cl, na.rm = TRUE)
        ) %>%
  	   mutate(active_year       = bal_peak > 0 ) %>%
	     select(RC.Account.Number, Year, bal_avg, bal_peak, active_year) %>%
	     right_join(clients, by = c('RC.Account.Number', 'Year'))
	   
	 
	   # fill in some things 
	   clients <- clients %>%
	     group_by(RC.Account.Number) %>% 
	     fill(sector, processing_type, active_today, rc_first, balance_first, 
	          loan_size_cat, loan_size_cat_scott, loan_count, Account.Name, .direction = c('down', 'up'))
	   
	   
	   clients$active_year[is.na(clients$active_year)] <- FALSE
	   # growth rates
	   clients <- clients %>%
	     group_by(RC.Account.Number) %>%
	     filter(!is.na(Year)) %>%
	     arrange(Year) %>%
	     mutate(sales_growth_yoy = (sales / lag(sales)) - 1 )	%>%
	     mutate(sales_growth_yoy = replace(sales_growth_yoy, is.infinite(sales_growth_yoy), NA),
	            year_n = Year - year_zero) %>%
	     ungroup() 
	   
	   # # Years of sales and CAGR
	   clients <- clients %>%
	     filter(Year >= year_zero, !is.na(sales) ) %>%
	     arrange(RC.Account.Number, Year) %>%
	     group_by(RC.Account.Number)  %>%
	     mutate(years_of_sales_data = sum(sales>=0, na.rm=TRUE)) %>%
	     ungroup() %>%
	     distinct(RC.Account.Number, years_of_sales_data) %>%
	     right_join(clients, by = 'RC.Account.Number') 
	   
	   clients <- clients %>%
	     filter(Year >= year_zero, !is.na(sales) ) %>%
	     arrange(RC.Account.Number, Year) %>%
	     group_by(RC.Account.Number)  %>%
	     summarise(
	       # year_zero_sales = max(min_sales_year, year_zero, na.rm=TRUE),   # if you want to just look for years of sales
	       year_last_sales = max(Year, na.rm=TRUE)
	       # years_of_sales_data = max(years_of_sales_data, na.rm=TRUE)
	     ) %>%
	     distinct(RC.Account.Number, year_last_sales) %>%
	     right_join(clients)

	   
	   # find sales in year_zero
	   clients <- clients %>%
	     filter(Year == year_zero) %>%  # gives 614 / 632
	     mutate(has_sales_zero = !is.na(sales)) %>%
	     rename(sales_in_year_zero = sales) %>%
	     select(RC.Account.Number, sales_in_year_zero, has_sales_zero) %>%
	     right_join(clients, by = c('RC.Account.Number'))

#------------------------------------
# MORE THINGS

# cagrs and end to end growth rate
	   
	   clients <- clients %>%
	     group_by(RC.Account.Number) %>%
	     filter(!is.na(sales), year_zero == TRUE | active_year == TRUE) %>%
	     summarise(max_sales_year = max(Year, na.rm = TRUE),
	               min_sales_year = min(Year, na.rm = TRUE)) %>%
	     right_join(clients, by = 'RC.Account.Number')
	   
	   clients <- clients %>%
	     filter(Year == year_zero | Year == max_sales_year) %>%      # note that this won't work perfectly for pmts and such
	     group_by(RC.Account.Number) %>%
	     arrange(Year) %>%
	     mutate(sales_growth = (sales / lag(sales, 1) - 1),
	            sales_cagr   = (sales_growth + 1)^(1/(max_sales_year - year_zero)) - 1,
	            payments_growth = (payments_to_producers / lag(payments_to_producers, 1) - 1),
	            producers_growth = (producers / lag(producers, 1) - 1)
	            ) %>%
	     # distinct(RC.Account.Number, sales_growth, sales_cagr, .keep_all = TRUE) %>%
	     filter(!is.na(sales_growth)) %>%
	     select(RC.Account.Number, sales_growth, sales_cagr, payments_growth, producers_growth) %>%
	     right_join(clients, by = 'RC.Account.Number')
	   
	   clients$cagr_animal <- cut(clients$sales_cagr,
	                                c(-Inf, 0, 0.20, Inf),
	                                labels = c('panda', 'antelope', 'gazelle'))
	   quantile(clients$sales_cagr, na.rm = TRUE, probs = seq(0, 1, by = 0.02))
	   
	   clients$growth_animal <- cut(clients$sales_growth,
	                         c(-Inf, 0, 0.30, Inf),
	                         labels = c('panda', 'antelope', 'gazelle'))
	   

# # write
# wd <- 'c:/Box Sync/jlittel/comms/client financials/data'
# setwd(wd)
# write.csv(clients, 'clients.csv')


pd_sales_graph <- ggplot(filter(df, sales < 1e7), aes(x = sales, y = pd)) + 
  geom_smooth(se = F, method = "lm", formula = y ~ splines::bs(x, 6)) + 
  scale_x_continuous(labels = scales::dollar) + geom_point(alpha = 0.05)