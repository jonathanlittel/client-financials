#--------------- PD MODEL ----------------------

# helper functions
replace_na <- function(x, replacement = 0) {
  # if(class(x) != 'numeric') stop('item must be numeric')
  y <- x
  y[is.na(x)] <- replacement
  y
}

# create dataframe of selected data
  df.pd <- select(lc, RC.Opp.Number, Loan.Tenor, Loan.Type )

  loans_sub <- select(loans, sales, RC.Opp.Number, WriteoffsDummy, pd) %>% 
    distinct(RC.Opp.Number, .keep_all = TRUE)

  df.pd <- right_join(df.pd, loans_sub, by = 'RC.Opp.Number')
 
  # add currency to loans df; for interest rate adjustment in rev estimate

  loans <- lc %>%
    select(RC.Opp.Number, Currency) %>%
    right_join(loans)

# create lagged sales feature  
df.pd <- select(loans, sales, RC.Opp.Number, WriteoffsDummy, pd, Year, year_one) %>% 
  group_by(RC.Opp.Number) %>% 
  arrange(Year) %>%
  fill(sales, .direction = c('down', 'up')) %>%
  mutate(sales_lag = lag(sales,1)) %>%
  filter(!is.na(sales_lag)) %>%
  # filter(Year == year_one) %>%
  select(RC.Opp.Number, sales_lag) %>%
  distinct(RC.Opp.Number, .keep_all = TRUE) %>%
  right_join(df.pd)

sum(is.na(df.pd$sales_lag))

# create a few more features and clean up NAs
  df.pd$sales_lag_log <- log(df.pd$sales_lag + 1)
  df.pd$WriteoffsDummy <- replace_na(df.pd$WriteoffsDummy, 0)
  df.pd$Loan.Tenor <- replace_na(df.pd$Loan.Tenor, median(df.pd$Loan.Tenor, na.rm = TRUE))

# recode interest rates below 7% to 7%, and non-USD rates to 10%
  loans$interest_rate_pred <- ifelse(loans$Internal.Interest.Rate..../ 100 < 0.07, 0.07, loans$Internal.Interest.Rate.... / 100)
  fx_interest_rate_usd_equiv <- 0.10
  loans$interest_rate_pred <- ifelse(loans$Currency=='USD', loans$interest_rate_pred, fx_interest_rate_usd_equiv)
  table(loans$Internal.Interest.Rate...., loans$interest_rate_pred)


# create pd based on lagged log of sales, and loan tenor
  # df.pd$sales_log <- replace_na(df.pd$sales_log, median(df.pd$sales_log, na.rm = TRUE))
  # df.pd$sales_lag_log <- replace_na(df.pd$sales_lag_log, median(df.pd$sales_lag_log, na.rm = TRUE))
  df.model <- select(df.pd, WriteoffsDummy, sales_lag_log, Loan.Tenor)
  glm.simple.pd <-glm(WriteoffsDummy ~ sales_lag_log, Loan.Tenor, data=df.model, family='binomial', na.action=na.exclude) 


# predict revenue (to replace revenue for writeoff loans)
  lc_cut <- select(lc, RC.Opp.Number, Loan.Tenor, Internal.Interest.Rate...., LoanID, Currency)
  n2 <- select(tx3, RC.Opp.Number, revenue) %>% left_join(lc_cut, by = 'RC.Opp.Number')
  n3 <- left_join(n2, wo, by = 'LoanID')
  df.rev <- select(bal4, RC.Opp.Number, bal_avg_loan) %>%
    distinct(RC.Opp.Number, .keep_all = TRUE) %>%
    right_join(n3, by = 'RC.Opp.Number')
  df.rev$WriteoffsDummy <- replace_na(df.rev$WriteoffsDummy, 0)
  # recode interest rates below 7% to 7%, and non-USD rates to 10%
  df.rev$interest_rate_pred <- ifelse(df.rev$Internal.Interest.Rate..../ 100 < 0.07, 0.07, df.rev$Internal.Interest.Rate.... / 100)
  fx_interest_rate_usd_equiv <- 0.10
  df.rev$interest_rate_pred <- ifelse(df.rev$Currency=='USD', df.rev$interest_rate_pred, fx_interest_rate_usd_equiv)

  # rev.lm <- lm(revenue ~ bal_avg_loan + Loan.Tenor + Internal.Interest.Rate.... , data = filter(df.rev, WriteoffsDummy == 0))   # using original interest rate
  rev.lm <- lm(revenue ~ bal_avg_loan + Loan.Tenor + interest_rate_pred ,           data = filter(df.rev, WriteoffsDummy == 0))

  revenue_predicted <- predict(rev.lm, df.rev, type = 'response')


# add predicted pd
  loans <- loans %>% group_by(RC.Opp.Number) %>% mutate(sales_lag = lag(sales,1, order_by = Year))
  loans$sales_lag_log <- log(loans$sales_lag + 1)
  loans$pd_w_imputation <- predict(glm.simple.pd, loans, type='response')
  loans$pd_w_imputation <- ifelse(is.na(loans$pd), loans$pd_w_imputation, loans$pd)

# add predicted revenue / yield
  # predicted revenue is peak balance, times 75% usage, for the loan tenor, times the interest rate (which is adjusted if <7%)
  usage_rate <- 0.75
  loans <- mutate(loans, revenue_predicted = bal_avg_loan * usage_rate * Loan.Tenor/12 * interest_rate_pred )
  plot(loans$revenue - loans$revenue_predicted)
  ggplot(loans, aes( x = revenue, y = revenue_predicted, color = WriteoffsDummy)) + geom_point()
  # revenue is payments over disbursements, so outstanding or other loans may have negative revenue
  # impute revenue for loans with less than 95% of disbursement repaid
  # loans$probably_complete <- ifelse(loans$yield > 0.90, TRUE, FALSE)
  # loans$revenue_estimate  <- ifelse(loans$probably_complete, loans$revenue, ....)
  loans$revenue_estimate <- ifelse(loans$WriteoffsDummy == 1 | loans$revenue < 0.95, loans$revenue_predicted, loans$revenue)


  loans$yield_ <- ifelse(loans$WriteoffsDummy == 1, loans$revenue_estimate / loans$disb, loans$yield)
  # plot(loans$yield, loans$yield_)

# add estimate revenue if sales

# add expected revenue, expected loss, interest cost et al back to loans dataframe
loans <- loans %>%
  mutate(
    ead = ifelse(Loan.Type == 'Line of Credit', 0.64, 0.54),
    lgd = ifelse(Loan.Use == 'Capital Expenditure', 0.69, 0.90),
    el  = bal_avg_loan * pd_w_imputation * ead * lgd,
    revenue_less_risk = revenue_estimate - el,
    interest_cost = Loan.Tenor/12 * 0.02 * bal_avg_loan,
    revenue_less_risk_less_debt = revenue_less_risk - interest_cost,
    revenue_less_risk_per_year = revenue_less_risk / ceiling(Loan.Tenor/12),   # divide by years in tenor, rounded up  - note that this doesn't do loans of <1 year that 'split' year boundaries, 
    # ie double counted 
    revenue_less_risk_less_debt = revenue_less_risk_less_debt,
    revenue_less_risk_less_debt_per_year = revenue_less_risk_less_debt * months_outstanding_per_year / months_outstanding
    )

# 17 loans without a loan type:
# sum(!is.na(loans$RC.Opp.Number[is.na(loans$Loan.Type)])) 

loans$revenue_less_risk_less_debt[is.na(loans$pd_w_imputation)] <- NA 
loans$revenue_less_risk_less_debt[loans$revenue_estimate==0] <- NA 


# robbie: use what have for collateral, then LoC are unsecured, capex are secured
# term EAD is 64% of max exposure, loc is 54%