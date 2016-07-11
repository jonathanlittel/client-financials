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
		distinct(Year, Account.Name)

# create new sales and purchases account with aggregated data
# Sales > revenue_sem > revenue_alex = order of preference
	fin <- fin %>%
			mutate(
				sales_a = ifelse(
					is.na(Sales),
					ifelse(!is.na(revenue_sem), revenue_sem,
						revenue_alex),
					Sales)
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