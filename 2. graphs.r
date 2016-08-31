
#----------------------------------------------	
##     graphs    #
#----------------------------------------------			

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

#--------------------------------------------
# Median growth by # years as root client
#--------------------------------------------
	# fin.sales.bal.or.year.zero <- fin.sales %>%
		# filter(balance>0 | year_zero==Year) %>% # switch to include years before loan
		# group_by(account) %>%
		# arrange(Year) %>%
		# # mutate(year_n = Year - year_zero) %>% # switch to include years before loan
		# mutate(year_n = row_number() - 1 ) %>% # counter of years borrowing (0 = year_zero)
		# mutate(sales_growth_yoy = (sales / lag(sales)) - 1 )	%>%
		# mutate(sales_growth_yoy = replace(sales_growth_yoy, is.infinite(sales_growth_yoy), NA)) %>%
		# ungroup() 

	growth_by_years <- fin.sales.bal.or.year.zero %>%
			arrange(year_n) %>%
			group_by(year_n) %>%
			# mutate(sales_growth_yoy = sales / lag(sales)) %>%
			summarise(
				sales_median = median(sales, na.rm=TRUE),
				year_over_year_growth_median = median( sales_growth_yoy, na.rm=TRUE),
				year_over_year_growth_mean = mean( sales_growth_yoy, na.rm=TRUE),
				n = n()
				)


			
    # boxplot and point ----------------------------------------------------
	# yr.graph <- ggplot(filter(fin.sales.bal.or.year.zero, year_over_year<5, year_over_year>-1.5, !is.na(year_over_year)),
	#  aes(x=year_n, y=year_over_year))
	# yr.graph + geom_jitter() + scale_y_continuous(labels = scales::percent) + geom_smooth() + geom_hline(yintercept=0, color='white')

	# yr.graph <- ggplot(filter(fin.sales.bal.or.year.zero, year_over_year<5, year_over_year>-1.5, !is.na(year_over_year)),
	#  aes(x=factor(year_n), y=year_over_year))
	# yr.graph + geom_boxplot(aes(group=factor(year_n))) +
	#  scale_y_continuous(labels = scales::percent) + geom_hline(yintercept=0, color='white') 

	# yr.graph <- ggplot(filter(growth_by_years, year_n >= -3), aes(x=year_n, y=year_over_year_growth_median))
	# yr.graph + geom_line() + scale_y_continuous(labels=scales::percent)

	pct_labels <- paste(
		prettyNum(growth_by_years$year_over_year_growth_median * 100, digits=2),
		"%", sep="")
	yr_labels <- levels(factor(growth_by_years$year_n))

	yr.graph <- ggplot(filter(growth_by_years, year_n >= -3), 
		aes(x=year_n, y=year_over_year_growth_median, label=pct_labels[3:length(pct_labels)]))
	yr.gr <- yr.graph + geom_point(alpha=0.35, color='blue') + 
		geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.10) +
		# ggtitle('Growth by # of Years as Client') +
		geom_line(alpha=0.2, color='blue') +
		scale_y_continuous('Year over year sales growth, median (percent)', labels=scales::percent) + 
		scale_x_continuous('Year', breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
	yr.gr


#---------------------------------------------
	# version including years before loan:
# Median growth by # years as root client
	fin.sales.bal.or.year.zero <- fin.sales %>%
		# filter for either having a balance or being the year before the loan
		# filter(balance>0 | year_zero==Year) %>% 
		group_by(account) %>%
		arrange(Year) %>%
		mutate(year_n = Year - year_zero) %>% # counter of years borrowing (0 = year_zero)
		# mutate(year_n = row_number() - 1 ) %>% # counter of years borrowing (0 = year_zero)
		mutate(year_over_year = (sales / lag(sales)) - 1 )	%>%
		mutate(year_over_year = replace(year_over_year, is.infinite(year_over_year), NA)) %>%
		ungroup() 
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

	version including years before loan:
	yr.graph <- ggplot(filter(growth_by_years, year_n >= -3), 
		aes(x=year_n, y=year_over_year_growth_median, label=pct_labels[4:length(pct_labels)]))
	yr.gr <- yr.graph + geom_point(alpha=0.35, color='blue') + 
		geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.10) +
		# ggtitle('Growth by # of Years as Client') +
		geom_line(alpha=0.2, color='blue') +
		scale_y_continuous('Year over year sales growth, median (percent)', labels=scales::percent) 
	yr.gr

#---------------------------------------------


			
#----------------------------------------------	
## relationship of payments sem to total cogs on financial statements
#----------------------------------------------			

lm.pmts <- lm(payments_sem ~ total_cogs + total_cogs^2, data = fin, na.action = na.omit)
summary(lm.pmts)
plot(fin$total_cogs / fin$payments_sem)
pmts_r <- fin$payments_sem / fin$total_cogs
pmts_r[is.infinite(pmts_r)] <- NA
summary(pmts_r)
median(pmts_r -1 , na.rm = TRUE)


#--------------------------
# write output 
#--------------------------	
	# this is used by markdown file to produce presentation
wd <- 'C:/Box Sync/jlittel/comms/client financials'
setwd(wd)
save.image('client_fin.Rdata')
# load('client_fin.Rdata')