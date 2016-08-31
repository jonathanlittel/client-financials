#--------------- PD MODEL ----------------------

names(lc)
df.pd <- select(lc, RC.Opp.Number, Loan.Tenor, Loan.Type )

loans_sub <- select(loans, sales, RC.Opp.Number, WriteoffsDummy, pd) %>% 
  distinct(RC.Opp.Number, .keep_all = TRUE)

df.pd <- right_join(df.pd, loans_sub, by = 'RC.Opp.Number')


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


df.pd$sales_lag_log <- log(df.pd$sales_lag + 1)


# df.pd$sales_log <- log(df.pd$sales + 1)
df.pd$WriteoffsDummy <- replace_na(df.pd$WriteoffsDummy, 0)
df.pd$Loan.Tenor <- replace_na(df.pd$Loan.Tenor, median(df.pd$Loan.Tenor, na.rm = TRUE))


# df.pd$sales_log <- replace_na(df.pd$sales_log, median(df.pd$sales_log, na.rm = TRUE))
# df.pd$sales_lag_log <- replace_na(df.pd$sales_lag_log, median(df.pd$sales_lag_log, na.rm = TRUE))
df.model <- select(df.pd, WriteoffsDummy, sales_lag_log, Loan.Tenor)
glm.simple.pd <-glm(WriteoffsDummy ~ sales_lag_log, Loan.Tenor, data=df.model, family='binomial', na.action=na.exclude) 


replace_na <- function(x, replacement = 0) {
  # if(class(x) != 'numeric') stop('item must be numeric')
  y <- x
  y[is.na(x)] <- replacement
  y
}

# predict revenue (to replace revenue for writeoff loans)
lc_cut <- select(lc, RC.Opp.Number, Loan.Tenor, Internal.Interest.Rate...., LoanID)
n2 <- select(tx3, RC.Opp.Number, revenue) %>% left_join(lc_cut, by = 'RC.Opp.Number')
n3 <- left_join(n2, wo, by = 'LoanID')
df.rev <- select(bal4, RC.Opp.Number, balance) %>%
  distinct(RC.Opp.Number, .keep_all = TRUE) %>%
  right_join(n3, by = 'RC.Opp.Number')
df.rev$WriteoffsDummy <- replace_na(df.rev$WriteoffsDummy, 0)
glimpse(df.rev)

rev.lm <- lm(revenue ~ balance + Loan.Tenor + Internal.Interest.Rate...., data = filter(df.rev, WriteoffsDummy == 0))

revenue_predicted <- predict(rev.lm, df.rev, type = 'response')
loans <- loans %>% group_by(RC.Opp.Number) %>% mutate(sales_lag = lag(sales,1, order_by = Year))
loans$sales_lag_log <- log(loans$sales_lag + 1)
loans$pd2 <- predict(glm.simple.pd, loans, type='response')
loans$pd2 <- ifelse(is.na(loans$pd), loans$pd2, loans$pd)
loans$interest_rate_pred <- ifelse(loans$Internal.Interest.Rate..../ 100 < 0.07, 0.07, loans$Internal.Interest.Rate.... / 100)
loans <- mutate(loans,revenue_predicted = balance * 0.7 * Loan.Tenor/12 * interest_rate_pred )
plot(loans$revenue - loans$revenue_predicted)
loans$revenue_ <- ifelse(loans$WriteoffsDummy == 1 | loans$revenue < 0, loans$revenue_predicted, loans$revenue)

loans$yield_ <- ifelse(loans$WriteoffsDummy == 1, loans$revenue_ / loans$disb, loans$yield)
plot(loans$yield, loans$yield_)

loans <- loans %>%
  mutate(
    ead = ifelse(Loan.Type == 'Line of Credit', 0.64, 0.54),
    lgd = ifelse(Loan.Type == 'Capital Expenditure', 0.69, 0.90),
    el  = balance * pd2 * ead * lgd,
    revenue_less_risk = revenue_ - el
  )
# quantile(loans_c$revenue_less_risk, probs = seq(0, 1, by = 0.01), na.rm = TRUE)
# robbie: use what have for collateral, then LoC are unsecured, capex are secured
# term EAD is 64% of max exposure, loc is 54%

# > sum(loans_c$pd == loans_c$pd2, na.rm = TRUE)
# [1] 1422
# > sum(!is.na(loans_c$pd2)
#       + )
# [1] 1781