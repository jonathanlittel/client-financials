

 # figure out if client dropped out or *could* be active in a year
 attrition_rates_by_year_n50k <- clients %>%
   group_by(RC.Account.Number) %>%
   filter(year_n > 0, loan_size_cat == '50k-500k', Year < 2016) %>%
   # filter(year_n > 0, between(sales, 0, 1e6), Year < 2016) %>%    # by sales category
   arrange(year_n) %>%
   mutate(active_next_year = lead(active_year, 1),
          active_next_year = replace(active_next_year, is.na(active_next_year), FALSE)) %>%
   group_by(year_n) %>%
   filter(active_year==TRUE, Year < 2015) %>%
   summarise(
     attrition_rate = 1 - (sum(active_next_year, na.rm = TRUE) / sum(active_year, na.rm = TRUE)),
     n = n())
  attrition_rates_by_year_n50k

 # figure out if loan dropped out or *could* be active in a year
 attrition_rates_by_loan_by_year_n50k <- loans %>%
   group_by(RC.Account.Number) %>%
   filter(loan_number > 0, loan_size_cat == '50k-500k', Year < 2016) %>%
   # filter(loan_number > 0, between(sales, 0, 1e6), Year < 2016) %>%   # by sales category
   distinct(loan_number, RC.Account.Number, .keep_all = TRUE ) %>%
   arrange(loan_number) %>%
   mutate(subsequent_loan = lead(loan_number, 1) > 0,
          subsequent_loan = replace(subsequent_loan, is.na(subsequent_loan), FALSE)) %>%
   group_by(loan_number) %>%
   filter(active_year==TRUE, Year < 2015) %>%
   summarise(
     attrition_rate_loan = 1 - (sum(subsequent_loan, na.rm = TRUE) / sum(loan_number>0, na.rm = TRUE)),
     n = n(),
     pd_median = median(pd, na.rm = TRUE),
     wo_mean = mean(WriteoffsDummy, na.rm = TRUE)
     )
   # figure out if loan dropped out or *could* be active in a year
 attrition_rates_by_loan_by_year_n_sales <- loans %>%
   group_by(RC.Account.Number) %>%
   # filter(loan_number > 0, loan_size_cat == '50k-500k', Year < 2016) %>%
   filter(loan_number > 0, between(sales_year, 0, 1e6), Year < 2016) %>%   # by sales category
   distinct(loan_number, RC.Account.Number, .keep_all = TRUE ) %>%
   arrange(loan_number) %>%
   mutate(subsequent_loan = lead(loan_number, 1) > 0,
          subsequent_loan = replace(subsequent_loan, is.na(subsequent_loan), FALSE)) %>%
   group_by(loan_number) %>%
   filter(active_year==TRUE, Year < 2015) %>%
   summarise(
     attrition_rate_loan = 1 - (sum(subsequent_loan, na.rm = TRUE) / sum(loan_number>0, na.rm = TRUE)),
     n = n(),
     pd_median = median(pd, na.rm = TRUE),
     wo_mean = mean(WriteoffsDummy, na.rm = TRUE)
     )


 attrition_rates_by_year_n_sales <- clients %>%
   group_by(RC.Account.Number) %>%
   filter(year_n > 0, between(sales_in_year_zero, 0, 1e6), Year < 2016) %>%
   # filter(year_n > 0, between(sales, 0, 1e6), Year < 2016) %>%    # by sales category
   arrange(year_n) %>%
   mutate(active_next_year = lead(active_year, 1),
          active_next_year = replace(active_next_year, is.na(active_next_year), FALSE)) %>%
   group_by(year_n) %>%
   filter(active_year==TRUE, Year < 2015) %>%
   summarise(
     attrition_rate = 1 - (sum(active_next_year, na.rm = TRUE) / sum(active_year, na.rm = TRUE)),
     n = n())
  attrition_rates_by_year_n_sales
 
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
 
 clients_bal_sum <- clients %>%
   filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
   group_by(year_n) %>%
   summarise(balance_total = sum(balance, na.rm = TRUE),
             balance_avg = mean(balance, na.rm = TRUE))
 
 temp <- attrition_rates_by_loan_by_year_n50k %>%
   rename(attrition_rate_loan_number = attrition_rate_loan) %>%
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
 

 summary_port <- clients5 %>%
   filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
   group_by(year_n, growth_animal) %>%
   summarise(
     producers_growth_median = median(producers_growth, na.rm = TRUE),
     producers_median = median(producers, na.rm = TRUE),
     sales_growth_median = median(sales_growth, na.rm = TRUE),
     sales_median = median(sales, na.rm = TRUE),
     payments_growth_median = median(payments_growth, na.rm = TRUE),
     payments_median = median(payments_to_producers, na.rm = TRUE)
   )

payments.plot <- ggplot(filter(summary_port, !is.na(growth_animal)), 
       aes(x = year_n, y = payments_median, group = growth_animal, color = growth_animal)) +
  geom_line()


#------------ functions to build portfolio 


  #------------ return the number of loans after five years, based on attrition rates, on 50k-500k loans
  find_n_loans <- function (n_loans, attrition_table = attrition_rates_by_year_n50k[,2]) {
    # this should be recursive ideally...
    y1 <- n_loans
    y2 <- n_loans * (1 - attrition_table[1,])
    y3 <- y2 * (1 - attrition_table[2,])
    y4 <- y3 * (1 - attrition_table[3,])
    y5 <- y4 * (1 - attrition_table[4,])
    migr <- rbind(   
      y1, y2, y3, y4, y5)
    row.names(migr) <- seq(1:5)
    colnames(migr) <- 'loans'
    migr <- round(migr, 0)
    as.vector(t(migr))
  }
  
  number_of_loans <- find_n_loans(10)
  
  #----------------- 
    # return_outcome <- function (dist, n_loans) {
    #   as.name(dist)
    #   sg_table <- clients %>% 
    #     filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
    #     group_by(year_n) %>% 
    #     summarise(quantiles = list(quantile( dist , na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    #   q <- t(as.data.frame(sg_table$quantiles))
    #   row.names(q) <- NULL
    #   sg_table <- cbind(sg_table, q)
    #   sg_table$quantiles <- NULL
    #   sg_table <- t(sg_table)
    #   colnames(sg_table) <- sg_table[1,]
    #   sg_table <- as.data.frame(sg_table)
    #   sg_table <- sg_table[-1,]
    # }
    # return_outcome("sales_growth", n_loans)
    # 

#--------- sales growth  ---------------
    sg_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(sales_growth, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(sg_table$quantiles))
    row.names(q) <- NULL
    sg_table <- cbind(sg_table, q)
    sg_table$quantiles <- NULL
    sg_table <- t(sg_table)
    colnames(sg_table) <- sg_table[1,]
    sg_table <- as.data.frame(sg_table)
    sg_table <- sg_table[-1,]

#--------- revenue ---------------
    rev_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue_est, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(rev_table$quantiles))
    row.names(q) <- NULL
    rev_table <- cbind(rev_table, q)
    rev_table$quantiles <- NULL
    rev_table <- t(rev_table)
    colnames(rev_table) <- rev_table[1,]
    rev_table <- as.data.frame(rev_table)
    rev_table <- rev_table[-1,]

#--------- risk ---------------
    risk_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(expected_loss, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(risk_table$quantiles))
    row.names(q) <- NULL
    risk_table <- cbind(risk_table, q)
    risk_table$quantiles <- NULL
    risk_table <- t(risk_table)
    colnames(risk_table) <- risk_table[1,]
    risk_table <- as.data.frame(risk_table)
    risk_table <- risk_table[-1,]
    
#---------rev less risk ---------------
    rev_risk_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue_less_risk, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(rev_risk_table$quantiles))
    row.names(q) <- NULL
    rev_risk_table <- cbind(rev_risk_table, q)
    rev_risk_table$quantiles <- NULL
    rev_risk_table <- t(rev_risk_table)
    colnames(rev_risk_table) <- rev_risk_table[1,]
    rev_risk_table <- as.data.frame(rev_risk_table)
    rev_risk_table <- rev_risk_table[-1,]
    
#--------- producers ---------------
    prod_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(producers, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(prod_table$quantiles))
    row.names(q) <- NULL
    prod_table <- cbind(prod_table, q)
    prod_table$quantiles <- NULL
    prod_table <- t(prod_table)
    colnames(prod_table) <- prod_table[1,]
    prod_table <- as.data.frame(prod_table)
    prod_table <- prod_table[-1,] 

#--------- wages ---------------
    wages_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(wages, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(wages_table$quantiles))
    row.names(q) <- NULL
    wages_table <- cbind(wages_table, q)
    wages_table$quantiles <- NULL
    wages_table <- t(wages_table)
    colnames(wages_table) <- wages_table[1,]
    wages_table <- as.data.frame(wages_table)
    wages_table <- wages_table[-1,] 
    
#--------- sales ---------------
    sales_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(sales, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(sales_table$quantiles))
    row.names(q) <- NULL
    sales_table <- cbind(sales_table, q)
    sales_table$quantiles <- NULL
    sales_table <- t(sales_table)
    colnames(sales_table) <- sales_table[1,]
    sales_table <- as.data.frame(sales_table)
    sales_table <- sales_table[-1,]       

#--------- payments ---------------
    payments_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(payments_to_producers, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(payments_table$quantiles))
    row.names(q) <- NULL
    payments_table <- cbind(payments_table, q)
    payments_table$quantiles <- NULL
    payments_table <- t(payments_table)
    colnames(payments_table) <- payments_table[1,]
    payments_table <- as.data.frame(payments_table)
    payments_table <- payments_table[-1,]       
    
#--------- payments grow ---------------
    pg_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(payments_growth, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(pg_table$quantiles))
    row.names(q) <- NULL
    pg_table <- cbind(pg_table, q)
    pg_table$quantiles <- NULL
    pg_table <- t(pg_table)
    colnames(pg_table) <- pg_table[1,]
    pg_table <- as.data.frame(pg_table)
    pg_table <- pg_table[-1,]       
    
#--------- balance ---------------
    balance_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(bal_avg, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(balance_table$quantiles))
    row.names(q) <- NULL
    balance_table <- cbind(balance_table, q)
    balance_table$quantiles <- NULL
    balance_table <- t(balance_table)
    colnames(balance_table) <- balance_table[1,]
    balance_table <- as.data.frame(balance_table)
    balance_table <- balance_table[-1,]       
    
#--------- sales increase ---------------
    clients$sales_increase <- clients$sales - clients$sales_in_year_zero
    sales_increase_table <- clients %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(sales_increase, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(sales_increase_table$quantiles))
    row.names(q) <- NULL
    sales_increase_table <- cbind(sales_increase_table, q)
    sales_increase_table$quantiles <- NULL
    sales_increase_table <- t(sales_increase_table)
    colnames(sales_increase_table) <- sales_increase_table[1,]
    sales_increase_table <- as.data.frame(sales_increase_table)
    sales_increase_table <- sales_increase_table[-1,]       
    

    number_of_loans <- find_n_loans(30)
    
#----------------------year 1
    q <- 1 / ((number_of_loans[1]) + 1)
    l_qu1 = round(
      seq(q, 1 - q, by = q), 
      digits = 2)
    
    rev_table[l_qu1*100,]
    sg_table[l_qu1*100,]
    sales_table[l_qu1*100,]
    balance_table[l_qu1*100,]
    prod_table[l_qu1*100,]
    wages_table[l_qu1*100,]
    risk_table[l_qu1*100,]
    rev_risk_table[l_qu1*100,]
    payments_table[l_qu1*100,]
    pg_table[l_qu1*100,]

    apply(prod_table[l_qu1*100,], 2, sum)
    apply(wages_table[l_qu1*100,], 2, sum)
    apply(sales_table[l_qu1*100,], 2, sum)
    apply(payments_table[l_qu1*100,], 2, sum)
    
#----------------------year 5
    find_n_loans(30)
    q <- 1 / ((number_of_loans[5]) + 1)
    l_qu = round(
      seq(q, 1 - q, by = q), 
      digits = 2)
    
    rev_table[l_qu*100,]
    sg_table[l_qu*100,]
    sales_table[l_qu*100,]
    balance_table[l_qu*100,]
    prod_table[l_qu*100,]
    wages_table[l_qu*100,]
    risk_table[l_qu*100,]
    rev_risk_table[l_qu*100,]
    payments_table[l_qu*100,]
    pg_table[l_qu*100,]
    
    apply(prod_table[l_qu*100,], 2, sum)
    apply(wages_table[l_qu*100,], 2, sum)
    apply(sales_table[l_qu*100,], 2, sum)
    apply(payments_table[l_qu*100,], 2, sum)
    apply(rev_risk_table[l_qu*100,], 2, sum)
    apply(rev_table[l_qu*100,], 2, sum)
    
   
    populate_row <- function(year) {
      loans <-  number_of_loans[year]
      q <- 1 / ((number_of_loans[year]) + 1)
      l_qu <- round(seq(q, 1-q, by = q), digits = 2)
      outcomes[year, 1] <- loans
      outcomes[year, 2] <- apply(sales_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 3] <- apply(prod_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 4] <- apply(payments_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 5] <- apply(wages_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 6] <- apply(rev_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 7] <- apply(risk_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 8] <- apply(rev_risk_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 9] <- apply(sales_increase_table[l_qu*100,], 2, sum)[year]
      outcomes[year, 10] <- apply(balance_table[l_qu*100,], 2, sum)[year]
      outcomes
    }
    
    outcomes <- data.frame(
      clients = rep(NA, 5),
      sales = rep(NA, 5),
      producer_years = rep(NA, 5),
      payments = rep(NA, 5),
      wages = rep(NA, 5),
      revenue_rc = rep(NA, 5),
      risk_rc = rep(NA, 5),
      rev_net_risk = rep(NA, 5),
      sales_increase = rep(NA, 5),
      balance = rep(NA, 5)
    )
    outcomes <- populate_row(year = 1)
    outcomes <- populate_row(year = 2)
    outcomes <- populate_row(year = 3)
    outcomes <- populate_row(year = 4)
    outcomes <- populate_row(year = 5)
    t(outcomes)    
    
    outcomes$sales / outcomes$clients
    outcomes <- outcomes %>%
      mutate(
        sales_cumulative = cumsum(sales),
        producer_years_cumulative = cumsum(producer_years),
        payments_cumulative = cumsum(payments),
        wages_cumulative = cumsum(wages)
      )
    
    rownames(outcomes) <- paste0('Y', 1:5)
    
    outcomes <- outcomes %>%
      mutate(
        opex = 25000 * clients,
        opex_cumulative = cumsum(opex),
        sales_per_opex_dollar = sales / opex,
        opex_dollar_per_producer_year = opex / producer_years ,
        sales_per_opex_dollar_cumulative = sales_cumulative / opex_cumulative,
        sales_increase_per_opex_dollar_cumulative = sales_increase / opex_cumulative
      )
    
    # outcomes <- round(outcomes, 2)
    t(outcomes)
    outcomes[-10] - outcomes_old
    




# balance by date, colored by sales size
  temp <- select(clients, sales, Year, RC.Account.Number)
  bal_gr <- left_join(bal2, temp, by = c('RC.Account.Number', 'Year'))

  bal_gr %>%
    arrange(sales) %>%
    filter(balance > 0) %>%
    ggplot(aes(x = date, y = balance, fill = sales/1e6)) + geom_bar(stat = 'identity')

# better colors 
balance_by_sales_all_graph <- bal_gr %>%
    fill(sales, .direction = 'down') %>%
    arrange(sales) %>%
    filter(balance > 0) %>%
    ggplot(aes(x = date, y = balance, fill = sales/1e6)) + geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_distiller(type = "seq", palette = 'Spectral', direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar")

# better colors 
balance_by_sales_5M_graph <- bal_gr %>%
    fill(sales, .direction = 'down') %>%
    arrange(sales) %>%
    filter(balance > 0, sales < 5000000) %>%
    ggplot(aes(x = date, y = balance, fill = sales/1e6)) + geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_distiller(type = "seq", palette = 'Spectral', direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar")
balance_by_sales_all_graph
balance_by_sales_5M_graph


temp <- filter(clients, !duplicated(RC.Account.Number))
temp %>% group_by(year_zero) %>% summarise( r = sum(has_sales_zero, na.rm = T) / n(), n())




      setwd("C:/Box Sync/jlittel/comms/client financials/data")
      save.image(file = 'clientfin4.RData')
      
      