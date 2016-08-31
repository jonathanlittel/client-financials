# construction a portfolio

 clients5_bal_sum <- clients5 %>%
   group_by(year_n) %>%
   summarise(balance_total = sum(balance, na.rm = TRUE),
             balance_avg = mean(balance, na.rm = TRUE))
 
 clients_bal_sum <- clients %>%
   filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
   group_by(year_n) %>%
   summarise(balance_total = sum(balance, na.rm = TRUE),
             balance_avg = mean(balance, na.rm = TRUE))
 
 temp <- attrition_rates_by_loan_by_year_n50k %>%
   rename(attrition_rate_loan_number = attrition_rate) %>%
   select(loan_number, attrition_rate_loan_number) %>%
   filter(loan_number < 6)
 
 portfolio_stat <- clients_bal_sum[1:5, c(1, 3)]
 portfolio_stat <- full_join(attrition_rates_by_year_n50k[1:5, 1:2], portfolio_stat, by = 'year_n')
 portfolio_stat <- cbind(portfolio_stat, temp)
 
 # expected loss
 el_sum <- loans %>%
   group_by(loan_number) %>%
   filter(active_year == TRUE, loan_size_cat == '50k-500k', !is.na(el)) %>%
   summarise(
     expected_loss_average = mean(el, na.rm = TRUE),
     expected_loss_median = median(el, na.rm = TRUE),
     el_over_peak_bal = sum(el, na.rm = TRUE) / sum(balance[!is.na(balance)], na.rm = TRUE),
     expected_revenue_average = mean(revenue_, na.rm = TRUE),
     # n_missing_revenue = sum(is.na(revenue_)),
     rev_less_proj_risk_median = median(revenue_ - el, na.rm = TRUE),
     rev_less_proj_risk_average = mean(revenue_ - el, na.rm = TRUE),
     n = n()
   )