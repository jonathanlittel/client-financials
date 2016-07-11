# first load and process data
	wd <- 'c:/Box Sync/jlittel/comms/client financials/'
	setwd(wd)
	source('merge and process.r')
	finx <- fin
# select columns
	fin <- select(finx, Year, sales=sales_a, account = Account.Name, purchases=Purchases.from.producers,
		totalCogs=Total.COGS, active, balance)

# Create summary stats
		sales_sum <- fin %>%
			filter(active == TRUE) %>%
			summarise(sales = sum(sales, na.rm=TRUE), purchases=sum(purchases, na.rm=TRUE),
				cogs=sum(totalCogs, na.rm=TRUE), n=n())

# Check number of unique clients in set
	n_accounts <- sum(!duplicated(fin$account))
	n_account_year <- sum(!duplicated(fin[,c('account', 'Year')])) # by client/year


# data completeness checks
	sum(is.na(fin$sales)) / dim(fin)[1]				   # percent missing sales
	sum(is.na(fin$sales[fin$balance>0])) / dim(fin)[1] # percent missing sales when there was a balance
	sum(fin$sales==0, na.rm=TRUE)                      # number with zero sales
	sum(fin$sales==0 & fin$balance>0, na.rm=TRUE)      # number with zero sales and a balance


# Find year of first balance and years active
 	y1 <- fin %>%
 			group_by(account) %>%
 			filter(active==TRUE) %>%
 			mutate(year_last_sales_data = max(Year)) %>%
 			mutate(year_one = min(Year))

 	y1 <- select(y1, account, Year, year_one, year_last_sales_data)
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
		select(account, Year, account, balance, sales, purchases, totalCogs, year_one:max_sales_year) %>%
	 	# filter(balance>=0, !is.na(sales) ) %>%
	 	arrange(account, Year) %>%
	 	group_by(account)  %>%
	 	mutate(years_of_sales_data = sum(sales>=0, na.rm=TRUE)) %>%
	 	mutate(year_zerox = year_one - 1) %>%
	 	summarise(year_zero = max(min_sales_year, year_zerox, na.rm=TRUE),
	 		years_of_sales_data = max(years_of_sales_data, na.rm=TRUE))

	# Merge in year zero (first year with both balance and sales data)
	    fin <- merge(fin, y0, by='account')
	    # remove other year calc cols
	    fin <- select(fin, -c(year_one, year_last_sales_data, min_sales_year))

	fin.sales <- fin %>%
				filter(!is.na(sales)) %>%
				arrange(account, Year) %>%
				fill(c(max_sales_year))

	fin.sales.bal <- fin %>%
				filter(!is.na(sales) & balance>0)

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


##################	
## summary stats #
##################			

# number of account-years is 2591
	n_account_years <- sum(!duplicated(fin[fin$balance>0,c('account', 'Year')]))
	n_clients <- 	sum(!duplicated(fin$account))
# cumulatives
	summary(fin.sales.bal$sales)
	summary(fin.sales.bal$purchases)
	bal_w_data <- data.frame(
		# sum_sales= prettyNum(sum(fin.sales.bal$sales), big.mark=','), # total sales for clients with data
		sum_sales= sum(fin.sales.bal$sales), # total sales for clients with data
		sum_purchases = sum(fin.sales.bal$purchases, na.rm=TRUE),		      # total purchases for clients with data
		percent_of_active_years_with_sales_data = dim(fin.sales.bal)[1] / n_account_years,       # proportion of client-years with data
		percent_of_clients = sum(!duplicated(fin.sales.bal$account)) / n_clients,  # proportion of clients (though not all years)            
		number_of_clients = n_clients
		)

# growth
	summary(cagrs$growth)
	summary(cagrs$sales_cagr)
	summary(cagrs$growth_sales)
	summary(cagrs$growth_purchases)
	
	growth_summary <- data.frame(
		cagr_average = prettyNum(mean(cagrs$sales_cagr[!is.infinite(cagrs$sales_cagr)], na.rm=TRUE), big.mark=','),
		cagr_median = median(cagrs$sales_cagr[!is.infinite(cagrs$sales_cagr)], na.rm=TRUE),
		growth_avg = mean(cagrs$growth_sales[!is.infinite(cagrs$growth_sales)], na.rm=TRUE),
		growth_median = median(cagrs$growth_sales[!is.infinite(cagrs$growth_sales)], na.rm=TRUE),
		cagr_infinite = sum(is.infinite(cagrs$sales_cagr)),
		percent_of_clients_w_data = sum(!duplicated(cagrs$account)) / n_clients,
		percent_of_client_years_w_data = dim(fin.sales.bal)[1] / n_account_years
		)

	sum(cagrs$sales[cagrs$Year==cagrs$year_zero]) # sales in 'year zero'
	sum(cagrs$sales[cagrs$Year==cagrs$max_sales_year]) # sales in most recent year

# cumulatives with imputed data

# impute missing data by rolling forward previous period to all future missing periods
	impute <- fin %>%
			group_by(account) %>%
			fill(sales, purchases, totalCogs) %>%
			ungroup()

	impute.sales.bal <- filter(impute, balance>0 & !is.na(sales))
	sum(impute$sales[impute$balance>0], na.rm=TRUE)
	sum(is.na(impute$sales[impute$balance>0])) / dim(impute)[1]

	bal_w_data_imputed <- data.frame(sum_sales= sum(impute.sales.bal$sales), # total sales for clients with data
		sum_purchases = sum(impute.sales.bal$purchases),		      # total purchases for clients with data
		pct_of_active_years_with_imputed_sales_data = dim(impute.sales.bal)[1] / n_account_years,       # proportion of client-years with data
		percent_of_clients = sum(!duplicated(impute.sales.bal$account)) / n_clients)  # proportion of clients (though not all years)            
	
	sales_sum_impute <-	impute %>%
			arrange(sales) %>%
			filter(balance>0) %>%
			summarise(sales_sum = sum(sales, na.rm=TRUE), n=n())


##################	
##     graphs    #
##################			

cagrs$grew <- as.factor(cagrs$sales_cagr>0) # category for coloring
cagrs <- cagrs %>% group_by(account) %>% fill(grew, .direction='up') # fill for NAs
cagrs <- cagrs %>% group_by(account) %>% fill(grew, .direction='down')

g <- ggplot(fin, aes(x=Year, y=sales))
g + geom_smooth() + geom_line(aes(group=account), alpha=0.25) + scale_y_log10()


g <- ggplot(filter(fin, sales<1e7), aes(x=Year, y=sales))
g  + geom_line(aes(group=account), alpha=0.1) + scale_y_log10()

# sales by client over year (for rmd presentation)
	g <- ggplot(filter(cagrs, sales<3e7), aes(x=Year, y=sales, group=account, color=grew)) 
	g + geom_line( alpha=0.25, size=0.5) 
	grow_graph <- g + geom_line( alpha=0.25, size=0.5) 


# simplified time axis
	cagrs$axis <- ifelse(cagrs$Year==cagrs$year_zero, 'Before Loan', 'Now')
	g <- ggplot(filter(cagrs, sales>=10000), aes(x=axis, y=sales, group=account, color=grew)) 
	# g + geom_line( alpha=0.25, size=0.5) + ggtitle('Growth in Sales while RC Client') + xlab("")
	grow_graph2 <- g + geom_line( alpha=0.20, size=0.6) + ggtitle('Growth in Sales while RC Client') + 
		xlab("") + ylab('Sales (log scale, <$1000 omitted)') + 
		scale_y_log10(labels = scales::dollar, breaks=(c(0,1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9))) +
		 scale_color_hue(l=45, c=35)
	grow_graph2
# plot cagr distribution
	cagrs_unique <- cagrs %>%
				filter(!is.na(sales_cagr) | !is.infinite(sales_cagr)) %>%
				filter(years_of_sales_data>2) %>%
				distinct(account)

	table(cagrs_unique$grew)

	k <- ggplot(filter(cagrs_unique, sales_cagr<5), aes(sales_cagr)) 
	k + geom_histogram()

	k <- ggplot(filter(cagrs_unique, sales_cagr<5), aes(x=1, sales_cagr))  # or x=grew
	k + geom_violin()


# to show:
bal_w_data
bal_w_data_imputed
growth_summary
	sum(cagrs$sales[cagrs$Year==cagrs$year_zero]) # sales in 'year zero'
	sum(cagrs$sales[cagrs$Year==cagrs$max_sales_year]) # sales in most recent year


# Median growth by # years as root client
	fin.sales.bal.or.year.zero <- fin.sales %>%
		# filter for either having a balance or being the year before the loan
		filter(balance>0 | year_zero==Year) %>% 
		group_by(account) %>%
		arrange(Year) %>%
		mutate(year_n = row_number() - 1 ) %>% # counter of years borrowing (0 = year_zero)
		mutate(year_over_year = (sales / lag(sales)) - 1 )	%>%
		mutate(year_over_year = replace(year_over_year, is.infinite(year_over_year), NA)) %>%
		ungroup() 


	yr.graph <- ggplot(filter(fin.sales.bal.or.year.zero, year_over_year<5, year_over_year>-1.5, !is.na(year_over_year)),
	 aes(x=year_n, y=year_over_year))
	yr.graph + geom_jitter() + scale_y_continuous(labels = scales::percent) + geom_smooth() + geom_hline(yintercept=0, color='white')

	yr.graph <- ggplot(filter(fin.sales.bal.or.year.zero, year_over_year<5, year_over_year>-1.5, !is.na(year_over_year)),
	 aes(x=factor(year_n), y=year_over_year))
	yr.graph + geom_boxplot(aes(group=factor(year_n))) +
	 scale_y_continuous(labels = scales::percent) + geom_hline(yintercept=0, color='white') 


	growth_by_years <- fin.sales.bal.or.year.zero %>%
			arrange(year_n) %>%
			group_by(year_n) %>%
			# mutate(year_over_year = sales / lag(sales)) %>%
			summarise(
				sales_median = median(sales, na.rm=TRUE),
				year_over_year_growth_median = median( year_over_year, na.rm=TRUE),
				year_over_year_growth_mean = mean( year_over_year, na.rm=TRUE),
				n = n()
				)

	yr.graph <- ggplot(growth_by_years, aes(x=year_n, y=year_over_year_growth_median))
	yr.graph + geom_line() + scale_y_continuous(labels=scales::percent)

pct_labels <- paste(
		prettyNum(growth_by_years$year_over_year_growth_median * 100, digits=2),
		"%", sep="")
yr_labels <- levels(factor(growth_by_years$year_n))
	yr.graph <- ggplot(growth_by_years, aes(x=year_n, y=year_over_year_growth_median, label=pct_labels))
	yr.gr <- yr.graph + geom_point(alpha=0.35, color='blue') + 
		geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.10) +
		# ggtitle('Growth by # of Years as Client') +
		geom_line(alpha=0.2, color='blue') +
		scale_y_continuous('Year over year growth rate, median (percent)', labels=scales::percent) + 
		scale_x_continuous('Year', breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
	yr.gr

wd <- 'C:/Box Sync/jlittel/comms/client financials'
setwd(wd)
save.image('client_fin.Rdata')
# load('client_fin.Rdata')