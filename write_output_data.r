# write output of expected loan loss and revenue
# identifiers:
#     data        | field name
# ----------------|---------------
# 1. loan         | RC.Opp.Number
# 2. loan, year   | RC.Opp.Number, Year
# 3. client, year | RC.Account.Number, Year 

# 1. one row per loan
# ----------------------------------------------------------------------------------------------------
loan_out1 <- loans %>%
  filter(!is.na(RC.Opp.Number | !is.na(Year))) %>%
  distinct(RC.Opp.Number, .keep_all = T) %>%
  select(RC.Opp.Number, Account.Name, 
 average_balance = bal_avg_loan, tenor = Loan.Tenor, interest_rate_estimate =  interest_rate_pred, probability_of_default = pd_w_imputation,
 ead, lgd, estimated_revenue = revenue_estimate,  interest_cost, expected_loss = el,  
 contribution_margin = revenue_less_risk_less_debt)

filename1 <- paste0('expected_revenue_loan_level_', Sys.Date(), '.csv')
write.csv(loan_out1, filename1, row.names = F)
file.show(filename1)



# 2. one row per loan, per year
# ----------------------------------------------------------------------------------------------------

loan_out2 <- loans %>%
  filter(!is.na(RC.Opp.Number)) %>%
  distinct(RC.Opp.Number, Year, .keep_all = T) %>%
  select(RC.Opp.Number, Account.Name, Year, months_outstanding_per_year,
 average_balance = bal_avg_loan, tenor = Loan.Tenor, interest_rate_estimate =  interest_rate_pred, probability_of_default = pd_w_imputation,
 ead, lgd, estimated_revenue = revenue_estimate,  interest_cost, expected_loss = el,  
 contribution_margin = revenue_less_risk_less_debt, 
 contribution_margin_per_year = revenue_less_risk_less_debt_per_year)

filename2 <- paste0('expected_revenue_loan_year_level_', Sys.Date(), '.csv')
write.csv(loan_out2, filename2, row.names = F)
file.show(filename2)

# 3. one row per client, per year
# ----------------------------------------------------------------------------------------------------

client_out1 <- clients %>%
  filter(!is.na(RC.Account.Number | !is.na(Year))) %>%
  distinct(RC.Account.Number, Year, .keep_all = T) %>%
  select(RC.Account.Number, Account.Name, Year,  active_year, # months_outstanding_per_year,
  estimated_revenue = revenue_estimate, expected_loss,
  interest_cost, expected_loss,  
  contribution_margin = revenue_less_risk_less_debt, 
  contribution_margin_per_year = revenue_less_risk_less_debt_per_year)

filename3 <- paste0('expected_revenue_client_year_level_', Sys.Date(), '.csv')
write.csv(client_out1, filename3, row.names = F)
file.show(filename3)
