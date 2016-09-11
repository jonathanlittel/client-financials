
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

attrition_rates_by_year_n50k
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
    # create distribution dataframe by year_n
    sg_table <- cl %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
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
    # create number of quantile slices
    # (alternatively could do all quantiles and use as lookup table...)
    q <- 1 / (length(number_of_loans) + 1)
    probs = round(
      seq(q, 1 - q, by = q), 
      digits = 2)
    
    # temp <- matrix(data = rep(NA, 5*10), nrow = 5, ncol = 10)
    # new <- for (y in 1:length(number_of_loans)) {
    #   print(y)
    #   q <- 1 / (number_of_loans[y] + 1 )
    #   print(q)
    #   probs = round(seq(q, 1 - q, by = q), digits = 2) * 100
    #   temp[y,] <- sg_table[probs, y]
    # }
    
    
#--------- revenue ---------------
    rev_table <- cl %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(rev_table$quantiles))
    row.names(q) <- NULL
    rev_table <- cbind(rev_table, q)
    rev_table$quantiles <- NULL
    rev_table <- t(rev_table)
    colnames(rev_table) <- rev_table[1,]
    rev_table <- as.data.frame(rev_table)
    rev_table <- rev_table[-1,]

#--------- risk ---------------
    risk_table <- cl %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(risk_table$quantiles))
    row.names(q) <- NULL
    risk_table <- cbind(risk_table, q)
    risk_table$quantiles <- NULL
    risk_table <- t(risk_table)
    colnames(risk_table) <- risk_table[1,]
    risk_table <- as.data.frame(risk_table)
    risk_table <- risk_table[-1,]

#--------- producers ---------------
    prod_table <- cl %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(prod_table$quantiles))
    row.names(q) <- NULL
    prod_table <- cbind(prod_table, q)
    prod_table$quantiles <- NULL
    prod_table <- t(prod_table)
    colnames(prod_table) <- prod_table[1,]
    prod_table <- as.data.frame(prod_table)
    prod_table <- prod_table[-1,] 

#--------- producers ---------------
    prod_table <- cl %>% filter(active_year == TRUE, loan_size_cat == '50k-500k') %>%
      group_by(year_n) %>% 
      summarise(quantiles = list(quantile(revenue, na.rm = TRUE, probs = seq(0.01, 1, by = 0.01))))
    q <- t(as.data.frame(prod_table$quantiles))
    row.names(q) <- NULL
    prod_table <- cbind(prod_table, q)
    prod_table$quantiles <- NULL
    prod_table <- t(prod_table)
    colnames(prod_table) <- prod_table[1,]
    prod_table <- as.data.frame(prod_table)
    prod_table <- prod_table[-1,]       
    
    
    
    rev_table[probs*100,]
    sg_table[probs*100,]
    