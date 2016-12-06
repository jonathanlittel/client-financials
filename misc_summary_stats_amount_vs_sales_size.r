# bryan's table
  lc_temp <- select(lc, RC.Opp.Number, Amount, date = date)
  lc_temp$Year <- year(lc_temp$date)
  l_temp <- select(loans, RC.Opp.Number, RC.Account.Number, sales, pd = pd2, Year, WriteoffsDummy)
  l_temp <- l_temp %>% 
  				group_by(RC.Account.Number) %>%
  				arrange(Year) %>%
  				fill(sales, .direction = 'down')

  s <- left_join(lc_temp, l_temp, by = c('RC.Opp.Number', 'Year'))

  s %>%
  	mutate(
  		amount_cat = cut(Amount, c(-Inf, 150000, 5e5, Inf)),
  		# sales_cat = cut(sales, 
				#  c(-Inf,2.5e5, 5e5, 1e6, 1.5e6, 2e6, 3e6, 4e6, 5e6, 1e7,2e7,5e7,Inf),
				#  labels = c('<= 250k', '250k-500k', '500k-1M', '1M-1.5M', '1.5M-2M', '2M-3M',
				#  '3M-4M', '4M-5M', '5M-10M', '10M-20M', '20M-50M', '50M+'))  		
  		sales_cat = cut(sales, 
				 c(-Inf, 1e6, 2.5e6, Inf),
				 labels = c('<1M', '1M-2.5M', '2.5M+'))
  		) %>%
  	group_by(sales_cat) %>%
  	summarise(
  		n = n(),
  		wo_mean = mean(WriteoffsDummy, na.rm = TRUE),
  		pd_mean = mean(pd, na.rm  = T)
  		)

  lc_temp <- select(lc, RC.Account.Number, RC.Opp.Number, Amount, date = date)
  lc_temp$Year <- year(lc_temp$date)
    # add in risk category
    risk_cat <- select(df.r_cat, date, risk_category, RC.Opp.Number)
    risk_cat <- risk_cat %>%
      filter(date == '2016-06-30')

  lc_temp <- lc_temp %>%
    left_join(risk_cat, by = 'RC.Opp.Number')
    # group_by(RC.Account.Number) %>%
    #   summarise(   # go down to one risk category per rc account number
    #   	risk_category = max(risk_category, na.rm = T))

  # lc_temp <- lc_temp %>% 
  #   group_by(Year, RC.Account.Number) %>% 
  #   summarise(Amount = max(Amount)) %>% 
  #   arrange(Year) %>%
  #   fill(Amount, .direction = 'down') 

    # balance by amount cat
  	bal_today %>%
  	left_join(lc_temp, by = c('RC.Opp.Number') ) %>%
  	mutate(
  		amount_cat = cut(Amount, c(-Inf, 150000, 5e5, Inf))
  		) %>%
  	group_by(amount_cat) %>%
  	summarise(
  		n = n(),
  		balance = sum(balance, na.rm = T)
  		)
  	# balance by risk cat
  	# note this is 8/31 balance and 6/30 risk cat
  	risk_cat_sum <- bal_today %>%
  	left_join(lc_temp, by = c('RC.Opp.Number') ) %>%
  	mutate(
  		risk_category = replace(risk_category, is.na(risk_category) | 0, 1),
  		amount_cat = cut(Amount, c(-Inf, 150000, 5e5, Inf)),
  		sales_cat = cut(sales, 
				 c(-Inf, 1e6, 2.5e6, Inf),
				 labels = c('<1M', '1M-2.5M', '2.5M+'))
  		) %>%
  	# group_by(sales_cat) %>%
  	group_by(amount_cat, at_risk = between(risk_category, 2, 4 )) %>%
  	summarise(
  		n = n(),
  		balance = sum(balance, na.rm = T),
  		balance_pct = sum(balance, na.rm = T)/8.8e7
  		)

  	apply(risk_cat_sum, 2, sum)