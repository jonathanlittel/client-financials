library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
# Read historical balance data
# https://rootcapital.my.salesforce.com/00O700000054Uu2
	wd <- 'c:/Box Sync/jlittel/comms/client financials/data'
	setwd(wd)
	bal <- read.csv('SF_balances_by_date_06.7.16.csv')
	bal <- select(bal, LoanID=Loan.ID, RC.Account.Number=RC.Account.Number, Account.Name=Account.Name,
		balance=Internal.Outstanding.Principal, date=Date)
	bal$date <- as.Date(as.character(bal$date), format='%m/%d/%Y')
	bal$Year <- as.numeric(year(bal$date))


# load rev data from social scorecards/alex db (from 1999)
		filename <- 'rev_db.csv'
		rev_alex <- read.csv(filename, skip=1)

# select and reshape rev_db long to wide	
		rev_alex <- rev_alex[-nrow(rev_alex)] # remove 'grand total' row
		rev_alex_long <- rev_alex %>%
				select(Account.Name, rev_1996:rev_2014) %>%
				gather(Year, revenue_alex, rev_1996:rev_2014)
		rev_alex_long$Year <- as.numeric(gsub('rev_', '', rev_alex_long$Year))


# load SEM rev and purchase data
		# https://rootcapital.my.salesforce.com/00O70000004xeLC
		# add RC Account Number field
		filename <- 'rev_sem.csv'
		rev_sem <- read.csv(filename)
		rev_sem <- rename(rev_sem, 
			Year = Data.Year, # rename year for merge key
			revenue_sem = Enterprise.Revenue..USD.,
			payments_sem = Payments.to.Producers..USD.) 

# read client financials		
	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
	setwd(wd)
	filename <-  "rap_data_Q4_15_05.24.16.csv"
	df.rap <- read.csv(filename, header=TRUE, sep=",")
	wd <- 'c:/Box Sync/jlittel/comms/client financials/'
	setwd(wd)
# setdiff(bal$Account.Name, df.rap$Account.Name)  <-- TODO spend some time checking missing / differences
# merge various sources of sales and purchases   --->  df.rev
	df.rev <- merge(x=df.rap, y=rev_alex_long, by=c('Account.Name', 'Year'), all.x=TRUE, all.y=TRUE)
	df.rev <- merge(x=df.rev, y=rev_sem, by=c('Account.Name', 'Year'), all.x=TRUE, all.y=TRUE)

	fin <- merge(bal, df.rev, by=c('Account.Name', 'Year'), all=TRUE)  # 3894 with all.x, 40856 with all
	
# merge balance by time, and financial data, on Year and Account (and loanID) --->  fin
	fin$balance[is.na(fin$balance)] <- 0 # NA balances from merge to zero

# Determine if active during Year
	fin <- fin %>%
		group_by(RC.Account.Number, Year) %>%
		mutate(bal = sum(balance, na.rm = TRUE)) %>%
		mutate(active = bal>0) %>%
		ungroup()
	fin$bal <- NULL

# remove duplicates of Year/account
	fin <- fin %>%
		distinct(Year, Account.Name, .keep_all = TRUE)

# create new sales and purchases account with aggregated data
# Sales > revenue_sem > revenue_alex = order of preference
	fin <- fin %>%
			mutate(
				sales_a = ifelse(
					is.na(Sales),
					ifelse(!is.na(revenue_sem), revenue_sem,
						revenue_alex),
					Sales),
				purchases_a = ifelse(
					is.na(Purchases.from.producers),
						payments_sem,
						Purchases.from.producers
						)			
				)

# remove years with no active balance & no data
	fin <- filter(fin, !(active==FALSE & is.na(Sales) & is.na(revenue_alex) & is.na(payments_sem)))

# Write output for faina
# to_faina <- select(fin, Account.Name, RC.Account.Number, active, Year,
# 	Sales, Total.Income, revenue_alex, revenue_sem, 
# 	Purchases.from.producers, Total.COGS, payments_sem)

# to_faina_filter <- filter(to_faina, !(active==FALSE & is.na(Sales) & is.na(revenue_alex) & is.na(payments_sem)))

# # omitted:
# omitted <- filter(to_faina, active==FALSE & is.na(Sales) & is.na(revenue_alex) & is.na(payments_sem))

# write.csv(to_faina_filter, 'sem_comparison.csv')
# file.show('sem_comparison.csv')	

#--------------------------------------------------------------------------------------
	# subset data

	finx <- fin
# select columns
	fin <- select(finx, Year, sales = sales_a, 
		account     = Account.Name, 
		purchases   = Purchases.from.producers,
		purchases_a = purchases_a,
		total_cogs  =Total.COGS,
		amount      = Amount,
		active, balance, payments_sem)

# Create summary stats
		sales_sum <- fin %>%
			filter(active == TRUE) %>%
			summarise(sales = sum(sales, na.rm=TRUE), purchases=sum(purchases, na.rm=TRUE),
				cogs=sum(total_cogs, na.rm=TRUE), n=n())

# Check number of unique clients in set
	n_accounts <- sum(!duplicated(fin$account))
	n_account_year <- sum(!duplicated(fin[,c('account', 'Year')])) # by client/year


# data completeness checks
	sum(is.na(fin$sales)) / dim(fin)[1]				   # percent missing sales
	sum(is.na(fin$sales[fin$balance>0])) / dim(fin[fin$balance>0])[1] # percent missing sales when there was a balance
	sum(fin$sales==0, na.rm=TRUE)                      # number with zero sales
	sum(fin$sales==0 & fin$balance>0, na.rm=TRUE)      # number with zero sales and a balance


# Find year of first balance and years active
 	y1 <- fin %>%
 			group_by(account) %>%
 			filter(active==TRUE) %>%
 			mutate(
 				year_one = min(Year),
 				last_year = max(Year),
 				years_active = last_year - year_one
 				)

 	y1 <- select(y1, account, Year, year_one, last_year)
 	fin <- merge(fin, y1, by=c('account', 'Year'), all.x=TRUE)


# Find first and last years of sales data
 	y2 <- fin %>%
	 	filter(!is.na(sales)) %>%
	 	group_by(account) %>%
	 	mutate(min_sales_year = min(Year), max_sales_year = max(Year))
 	y2 <- select(y2, account, Year, min_sales_year, max_sales_year)
  	fin <- merge(fin, y2, by=c('account', 'Year'), all.x=TRUE)

# Years of sales and CAGR
	y0 <- fin %>%
		select(account, Year, account, balance, sales, purchases, total_cogs, year_one:max_sales_year) %>%
	 	# filter(balance>=0, !is.na(sales) ) %>%
	 	arrange(account, Year) %>%
	 	group_by(account)  %>%
	 	mutate(years_of_sales_data = sum(sales>=0, na.rm=TRUE)) %>%
	 	mutate(year_zerox = year_one - 1) %>%
	 	summarise(
	 		year_zero = max(min_sales_year, year_zerox, na.rm=TRUE),
	 		years_of_sales_data = max(years_of_sales_data, na.rm=TRUE)
	 		)

	# Merge in year zero (first year with both balance and sales data)
	    fin <- merge(fin, y0, by='account')
	    # remove other year calc cols
	    fin <- select(fin, -c(year_one, min_sales_year))

	fin.sales <- fin %>%
				filter(!is.na(sales)) %>%
				arrange(account, Year) %>%
				fill(c(max_sales_year))

	fin.sales.bal <- fin %>%
				filter(!is.na(sales) & balance>0)

	# function to calculate the row over previous row growth rate of x			
	gro_rate <- function(x) {x/lag(x)  }

		 cagrs <- fin.sales %>%
		 	arrange(account, Year) %>%
		 	fill(year_zero, .direction='up') %>%
		 	fill(account, .direction='up') %>%
		 	filter(Year==year_zero | Year==max_sales_year) %>%
		 	arrange(account, Year) %>%
		 	group_by(account) %>%
		 	mutate(growth_sales = gro_rate(sales) - 1) %>%
		 	mutate(growth_purchases = gro_rate(purchases) - 1) %>%
		 	mutate(sales_cagr = (growth_sales + 1) ^ (1 / (max_sales_year - year_zero)) - 1)

# summaries of clients with multiple years of loans
	# multi <- filter(growth, years_of_sales_data>3, !is.infinite(sales_growth), sales_growth<=100)
	# multi_hi <- filter(growth, years_of_sales_data>3, is.infinite(sales_growth) | sales_growth>100)
	# summary(multi$years_of_sales_data)
	# summary(multi$sales_CAGR)
	# summary(multi$sales_growth)
	# sum(!duplicated(multi$account))

