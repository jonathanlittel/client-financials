df <- fin.sales.bal.or.year.zero
table(duplicated(fin.sales.bal.or.year.zero[,c('account', 'Year')]))

# df <- df %>%
# 		group_by(account) %>%
# 		arrange(Year) %>%
# 		mutate(
# 			growth_yoy = sales / lag(sales),
# 			growth_yoy = replace(growth_yoy, is.infinite(growth_yoy), NA),
# 			growth_yoy = replace(growth_yoy, growth_yoy>20, 20)
# 			) 
year_over_year
#-------------------------------------------------
# linear model for estimated sales growth
#-------------------------------------------------
	# linear model to impute sales growth by size and time with root
	# need to create either year_n in cagrs, or cagrs in fin.sales.bal.or.year.zero 
	# temp_cagrs <- select(cagrs, account, Year, sales_cagr)
	# reg.data <- merge(df, temp_cagrs, by=c('account', 'Year'))
	# lm.gr <- lm(sales_cagr ~ sales_cagr + year_n , data = fin.sales.bal.or.year.zero, na.action = na.rm)


lm <- lm(sales ~ year_n + lag(sales), data = df, na.action = na.omit)

# lm <- lm(year_over_year ~ year_n + sales + purchases + sales^2, data = df, na.action = na.omit)

