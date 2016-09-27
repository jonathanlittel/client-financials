
# subset to the second to last year, if we have sales data
# cl - one row per client, for the year where we have sales data
# dim(cl) =  409
cl <- clients %>%
  filter(
    years_of_sales_data == year_n,  # or year_n - 1, if you want to skip y0..
    loan_size_cat == '50k-500k'
    # loan_size_cat_scott == '50k-200k'
  )

# > table(cl$years_of_sales_data)
# 
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 
# 93 57 39 28 25 16 14 11  9  6  3  6  2  1 

cl_sum <- cl %>%
  group_by(year_n, growth_animal) %>%      # group_by rc_first, animal
  summarize(
    cagr_median = median( sales_cagr, na.rm = TRUE),
    sales_growth_median = median(sales_growth, na.rm = TRUE),
    producers_growth_median = median(producers_growth, na.rm = TRUE), 
    cagr_75p = quantile(sales_cagr, na.rm = TRUE, probs = 0.75),
    sales_growth_75p = quantile(sales_growth, na.rm = TRUE, probs = 0.75),
    producers_growth_75p = quantile(producers_growth, na.rm = TRUE, probs = 0.75),
    n = n()
  )

#--------------- simple growth plots by group -----------------
# these graphs have one observation per client *only* not one per year
# only the year we last had data, if it was the year 
ggplot(cl_sum, aes(x = year_n, y = cagr_median)) +
# ggplot(cl_sum, aes(x = year_n, y = cagr_median)) + 
  # geom_line(aes(group = rc_first, color = rc_first)) + 
  geom_line(aes(group = growth_animal, color = growth_animal)) + 
  geom_line()

q20 <- seq(0.2, 0.8, by = 0.2)
q20 <- c(.2, .4, .5, .6, .8)
q20 <- c(0.25, 0.5, 0.75)
# cl$mask[cl$sales_growth > 5 ] <- 1
sales.growth.quadrants <- ggplot(cl, aes(x = year_n, y = sales_growth)) + 
  # geom_violin(alpha = 0.25, aes(group = year_n)) + 
  geom_jitter(width = 0.05, alpha = 0.2, size = 0.5) + 
  geom_quantile(quantiles = q20, alpha = 0.3, size = 1.5) +   # to add smoothing: method = "rqss", lambda  = 0.75
  scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(ylim=c(min(cl$sales_growth, na.rm = TRUE) , 10)) + # cut axis without removing data
  # facet_grid(. ~ mask, scales="free", space="free")
  theme_minimal()
sales.growth.quadrants


prod.growth.quadrants <- ggplot(filter(cl, year_n < 7), aes(x = year_n, y = producers_growth)) + 
  # geom_violin(alpha = 0.25, aes(group = year_n)) + 
  geom_jitter(width = 0.05, alpha = 0.2, size = 0.5) + 
  geom_quantile(quantiles = q20, alpha = 0.3, size = 1.5) +   # to add smoothing: method = "rqss", lambda  = 0.75
  scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(ylim=c(min(cl$sales_growth, na.rm = TRUE) , 2.5)) + # cut axis without removing data
  # facet_grid(. ~ mask, scales="free", space="free")
  theme_minimal()
prod.growth.quadrants

wages.growth.quadrants <- ggplot(filter(cl, year_n < 7), aes(x = year_n, y = wages)) + 
  # geom_violin(alpha = 0.25, aes(group = year_n)) + 
  geom_jitter(width = 0.05, alpha = 0.2, size = 0.5) + 
  geom_quantile(quantiles = q20, alpha = 0.3, size = 1.5) +   # to add smoothing: method = "rqss", lambda  = 0.75
  scale_y_continuous(labels = scales::dollar) + 
  coord_cartesian(ylim=c(min(cl$sales_growth, na.rm = TRUE) , 1e5)) + # cut axis without removing data
  # facet_grid(. ~ mask, scales="free", space="free")
  theme_minimal()
wages.growth.quadrants

# prod.growth.quadrants <- ggplot(cl_sum, aes(x = year_n, y = sales_growth_median, group = growth_animal, color = growth_animal)) + 
#   # geom_violin(alpha = 0.25, aes(group = year_n)) + 
#   geom_line(alpha = 0.2, size = 3) + 
#   geom_quantile(quantiles = q20, alpha = 0.3, size = 1) +   # to add smoothing: method = "rqss", lambda  = 0.75
#   scale_y_continuous(labels = scales::percent)

table(cl$years_of_sales_data, cl$year_n)
active_v_data <- data.frame(table(clients$years_of_sales_data, clients$year_n))
active_v_data2 <- melt(active_v_data)
ggplot(active_v_data, aes()) + geom_tile()


clients %>% group_by(Year) %>% filter(year_n==1, active_year==TRUE) %>% 
  summarise(
  year_fraction = 2016 - min(Year) + 1,
  retention25 = quantile(years_active, na.rm = TRUE, probs = 0.25) / year_fraction,
  retention50 = quantile(years_active, na.rm = TRUE, probs = 0.50) / year_fraction,
  retention75 = quantile(years_active, na.rm = TRUE, probs = 0.75) / year_fraction,
  retention90 = quantile(years_active, na.rm = TRUE, probs = 0.90) / year_fraction, 
  n = n()
)


 p <- ggplot(filter(clients, Year==year_one, loan_size_cat=='50k-500k'), aes(x = Year)) +
 # p <- ggplot(filter(clients, Year==year_one ), aes(x = Year)) +
 # ggplot(filter(clients, Year==year_one), aes(x = Year, fill = loan_size_cat)) +
   # geom_histogram(binwidth = 1, alpha = 0.5) +
   geom_histogram(bins = 18, alpha = .35, position = "stack") +
   stat_bin(aes(y=..count.., label=..count..), binwidth = 1, geom="text", vjust=-.5, bins = 18) +
   theme_minimal()
p
# gg <- ggplotly(p)

# for scott

count.by.cat <- ggplot(filter(clients, Year==year_one), aes(x = Year, fill = loan_size_cat)) +
  # geom_histogram(binwidth = 1, alpha = 0.5) +
  geom_histogram(bins = 18, alpha = .35, position = "stack") +
  stat_bin(aes(y=..count.., label=..count..), binwidth = 1, geom="text", vjust=-.5, bins = 18) +
  theme_minimal()


  scott <- clients %>% filter(Year == year_one) %>% select(Account.Name, Year, year_one, loan_size_cat, loan_size_cat_scott, balance)
  scott2 <- clients %>% filter(Year == year_one) %>%
    group_by(Year, loan_size_cat) %>%
    summarize(n = n())
  write.csv(scott2, 'scott2.csv')
  file.show('scott.csv')


summary(clients$balance)
names(bal2)

