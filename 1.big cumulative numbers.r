# first load and process data
	wd <- 'c:/Box Sync/jlittel/comms/client financials/'
	setwd(wd)
	source('0. merge and process.r')

#----------------------------------------------	
## summary stats #
#----------------------------------------------			

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
		percent_of_active_years_with_sales_data = dim(fin.sales.bal)[1] / n_account_years,       # proportion of client-years with sales data
		percent_of_active_years_with_purchases_data = dim(fin %>%
				filter(!is.na(purchases) & balance > 0))[1] / n_account_years,       # proportion of client-years with purchases data
		percent_of_clients = sum(!duplicated(fin.sales.bal$account)) / n_clients,  # proportion of clients (though not all years)            
		number_of_clients = n_clients
		)
	t(bal_w_data)
# index of those with infinite sales growth 
	inf_cagrs_index <- is.infinite(cagrs$sales_cagr)
	table(inf_cagrs_index)
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

# impute missing data by rolling forward previous period to all future missing periods
	impute <- fin %>%
			group_by(account) %>%
			mutate(sales_pre_impute = sales) %>%
			fill(sales, purchases, total_cogs) %>%
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

