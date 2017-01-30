library(ggthemes)
library(dplyr)
library(ggplot2); library(xlsx)

# -------------- load data --------------------------------------------------------
wd <- choose.dir(caption = 'Select the folder with the clients_DATE.csv file')
# wd <- 'C:/Box Sync/jlittel/comms/client financials/data/graphs/for mike'
  setwd(wd)
  filename <- 'clients_2017-01-26.csv'
  clients <- read.csv(filename)

# -------------- features --------------------------------------------------------
# add running sales growth pct
clients <- clients %>% group_by(RC.Account.Number) %>%
  arrange(Year) %>%
  mutate(sales_growth_running_pct = sales / sales_in_year_zero - 1) %>%
  ungroup()

  clients %>% filter(year_one == 2006) %>% arrange(Year) %>% select(Year) %>% group_by(Year) %>% summarise(n())
  clients$Account.Name <- as.factor(clients$Account.Name)
  clients <- clients %>% group_by(RC.Account.Number) %>% fill(Account.Name, .direction = c( 'down', 'up'))
  
# add count of clients in vintage year 
  vintage_count <- clients %>%
    ungroup() %>%
    filter(active_year == TRUE) %>%
    distinct(RC.Account.Number, year_one) %>%
    group_by(year_one) %>%
    summarise(client_count_in_vintage_year = n())
  
  clients <- clients %>% left_join(vintage_count, by = 'year_one')
  
# set save directory
  output_dir <- 'C:/Box Sync/jlittel/comms/client financials/output/for mike'
  setwd(output_dir)
  
# set some plot parameter standards
  plot_width = 7.5
  plot_height = 4.5

# -----------------------------------------------------------------------------------------------------------------
# ------------------------------------------- 2006 vintage - nominal growth ---------------------------------------
# -----------------------------------------------------------------------------------------------------------------
# select just clients who started in 2006
clients2006 <- clients %>% filter(year_one == 2006)

# plot 2006 vintage
p <- clients2006 %>%
  filter(Year >= 2005, sales_in_year_zero < 4.5e6, Year <= 2015) %>%
  ggplot(aes(x = Year, y = sales/1e6)) + 
  scale_y_continuous('Sales (Millions)', labels = scales::dollar) +
  scale_x_continuous(labels = function(n){format(round(n), scientific = FALSE)}, breaks = seq(2005, 2015, 1)) +
  scale_colour_hue(guide = "none") +   # remove legend
  theme_bw() 

# graph without median line
p_2006_no_line <- 
  p + geom_line(aes(group = RC.Account.Number, color = as.factor(RC.Account.Number)), alpha = 0.75) 

p_2006_median_line <- 
  p + geom_line(aes(group = RC.Account.Number, color = as.factor(RC.Account.Number)), alpha = 0.75) +
  stat_summary(aes(colour = "median", group = 1), fun.y = median, geom = "line", size = 4, alpha = 0.6, color = 'blue')

p_2006_no_line
p_2006_median_line

ggsave(paste0('2006_vintage_clients_sales_growth_nominal_no_median_', Sys.Date(), '.png'),
  plot = p_2006_no_line, width = plot_width, height = plot_height)

ggsave(paste0('2006_vintage_clients_sales_growth_nominal_', Sys.Date(), '.png'),
  plot = p_2006_median_line, width = plot_width, height = plot_height)

# -----------------------------------------------------------------------------------------------------------------
# ------------------------------------------- 2006 vintage - pct growth ---------------------------------------
# -----------------------------------------------------------------------------------------------------------------

# plot 2006 vintage
p_pct <- clients2006 %>%
  # filter(Year >= 2005, sales_in_year_zero < 4.5e6, Year <= 2015) %>%
  filter(Year >= 2005, Year <= 2016) %>%
  ggplot(aes(x = Year, y = sales_growth_running_pct)) + 
  scale_y_continuous('Sales Growth', labels = scales::percent, 
    # breaks = seq(-1, max(clients2006$sales_growth_running_pct, na.rm = TRUE), length.out = 6)) +
    breaks = c(0, 10, 20, 30, 40, 50)) +
  scale_x_continuous(labels = function(n){format(round(n), scientific = FALSE)}, breaks = seq(2005, 2015, 1)) +
  scale_colour_hue(guide = "none") +   # remove legend
  theme_bw() 

# graph without median line
p_2006_no_line <- 
  p_pct + geom_line(aes(group = RC.Account.Number, color = as.factor(RC.Account.Number)), alpha = 0.75) 

p_2006_median_line <- 
  p_pct + geom_line(aes(group = RC.Account.Number, color = as.factor(RC.Account.Number)), alpha = 0.75) +
  stat_summary(aes(colour = "median", group = 1), fun.y = median, geom = "line", size = 4, alpha = 0.6, color = 'blue')

p_2006_no_line
p_2006_median_line

ggsave(paste0('2006_vintage_clients_sales_growth_pct_no_median_', Sys.Date(), '.png'),
  plot = p_2006_no_line, width = plot_width, height = plot_height)

ggsave(paste0('2006_vintage_clients_sales_growth_pct_', Sys.Date(), '.png'),
  plot = p_2006_median_line, width = plot_width, height = plot_height)

# -----------------------------------------------------------------------------------------------------------------
# -------------------------------------------sales growth $          ----------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

client_nom_growth <- clients %>%
  ungroup() %>%
  dplyr::filter(Year >= year_one - 1, Year <= 2014, Year > 2000, year_n < 8, year_n >= 0) %>%
  filter(year_one <= 2012, year_one >= 2006) %>%
  mutate(`Vintage Year` = as.factor(year_one)) %>%
  group_by(year_n, `Vintage Year`) %>%
  summarize(sales_50 = quantile(sales, probs = 0.50, na.rm = T),
            sales_25 = quantile(sales, probs = 0.25, na.rm = T),
            sales_75 = quantile(sales, probs = 0.75, na.rm = T),
            n = n(),
            client_count_in_vintage_year = max(client_count_in_vintage_year),
            n_w_sales = n - sum(is.na(sales)),
            pct_w_sales = round(n_w_sales / max(client_count_in_vintage_year), 2)
            ) 

  p_all_vintages <- client_nom_growth %>%
    ggplot(aes( x = year_n, y = sales_50/1e6)) +
    geom_line(aes(group = `Vintage Year`, color = `Vintage Year`), size = 1, alpha = 1) + 
    # geom_text(aes(label = pct_w_sales)) +
    # geom_smooth(aes(group = as.factor(year_one), color = as.factor(year_one))) + 
    geom_smooth(size = 0.1, color = 'grey', alpha = 0.45, method = 'lm') + 
    scale_y_continuous('Median Client Sales (Millions)', labels = scales::dollar) +
    scale_x_continuous('Years as Client') +
    theme_hc()

ggsave(paste0('sales_growth_by_vintage_', Sys.Date(), '.png'), plot = p_all_vintages, width = plot_width, height = plot_height)

# -----------------------------------------------------------------------------------------------------------------
# -------------------------------------------sales growth running pct----------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

client_pct_growth <- clients %>%
    ungroup() %>%
    dplyr::filter(Year >= year_one - 1, Year <= 2016, Year > 2000, year_n < 8, year_n >= 0) %>%
    filter(year_one <= 2012, year_one >= 2006) %>%
    mutate(`Vintage Year` = as.factor(year_one)) %>%
    group_by(year_n, `Vintage Year`) %>%
    summarize(sales_50 = quantile(sales_growth_running_pct, probs = 0.50, na.rm = T),
              sales_25 = quantile(sales_growth_running_pct, probs = 0.25, na.rm = T),
              sales_75 = quantile(sales_growth_running_pct, probs = 0.75, na.rm = T),
              n = n(),
              client_count_in_vintage_year = max(client_count_in_vintage_year),
              n_w_sales = n - sum(is.na(sales_growth_running_pct)),
              label_field = paste(round(n_w_sales / max(client_count_in_vintage_year), 2),'x')
              ) 

  p_running_pct <- client_pct_growth %>%
    # ggplot(aes( x = Year, y = sales_50)) +
    ggplot(aes( x = year_n, y = sales_50)) +
    geom_line(aes(group = `Vintage Year`, color = `Vintage Year`), size = 1, alpha = 1) + 
    # geom_text(aes(label = label_field)) +
    # geom_smooth(aes(group = as.factor(year_one), color = as.factor(year_one))) + 
    geom_smooth(size = 0.1, color = 'grey', alpha = 0.45, method = 'lm') + 
    scale_y_continuous('Median Client Sales Growth', labels = scales::percent) +
    scale_x_continuous('Years as Client', breaks = 0:7) +
    theme_hc()    # theme_hc()

    
  p_running_pct

ggsave(paste0('sales_growth_by_vintage_running_pct_7_years_', Sys.Date(), '.png'), plot = p_running_pct, width = plot_width, height = plot_height)

# -----------------------------------------------------------------------------------------------------------------
# -------------------------------------------sales growth running pct all years -----------------------------------
# -----------------------------------------------------------------------------------------------------------------

  p_running_pct <- clients %>%
    ungroup() %>%
    dplyr::filter(Year >= year_one - 1, Year <= 2016, Year > 2000, year_n < 100, year_n >= 0) %>%
    filter(year_one <= 2012, year_one >= 2006) %>%
    mutate(`Vintage Year` = as.factor(year_one)) %>%
    group_by(year_n, `Vintage Year`) %>%
    summarize(sales_50 = quantile(sales_growth_running_pct, probs = 0.50, na.rm = T),
              sales_25 = quantile(sales_growth_running_pct, probs = 0.25, na.rm = T),
              sales_75 = quantile(sales_growth_running_pct, probs = 0.75, na.rm = T),
              n = n(),
              client_count_in_vintage_year = max(client_count_in_vintage_year),
              n_w_sales = n - sum(is.na(sales_growth_running_pct)),
              label_field = paste(round(n_w_sales / max(client_count_in_vintage_year), 2),'x')
              ) %>%
    # ggplot(aes( x = Year, y = sales_50)) +
    ggplot(aes( x = year_n, y = sales_50)) +
    geom_line(aes(group = `Vintage Year`, color = `Vintage Year`), size = 1, alpha = 1) + 
    # geom_text(aes(label = label_field)) +
    # geom_smooth(aes(group = as.factor(year_one), color = as.factor(year_one))) + 
    geom_smooth(size = 0.1, color = 'grey', alpha = 0.45, method = 'lm') + 
    scale_y_continuous('Median Client Sales Growth', labels = scales::percent, breaks = c(0, 2.5, 5, 7.5, 10, 12.5)) +
    scale_x_continuous('Years as Client', breaks = seq(0, 18, 2)) +
    theme_hc()    # theme_hc()

    
  p_running_pct

ggsave(paste0('sales_growth_by_vintage_running_pct_all_years_', Sys.Date(), '.png'), plot = p_running_pct, width = plot_width, height = plot_height)

# # calculate attrition rates by vintage, and cumulative attrition rates
# # complete matrix for all years for all clients
# full_year_grid <- expand.grid(1998:2014, unique(clients$RC.Account.Number))
# names(full_year_grid) <- c('Year', 'RC.Account.Number')

# # expand out years to include all years, then filter back to only year before active, and the year after the last year of lending
# clients.expand <- clients %>% 
#   select(RC.Account.Number, Year, year_zero, year_one_old = year_one, active_year, year_last) %>% 
#   full_join(full_year_grid) %>%
#   mutate(year_n = Year - year_zero) %>%
#   filter(Year > year_zero, Year <= year_last + 1)

# attrition_table <- clients.expand %>%
#   filter(Year > year_zero, Year <= 2015 ) %>%
#   mutate(possible_year = year_last - Year) %>%
#   group_by(year_n) %>%
#   arrange(Year) %>%
#   summarise(
#     n = n(),
#     n_possible_years = sum(possible_year, na.rm = T),
#     sum(active_year, na.rm = T))


# -----------------------------------------------------------------------------------------------------------------
# -------------------------------------------         attrition rates           -----------------------------------
# -----------------------------------------------------------------------------------------------------------------

 # figure out if client dropped out or *could* be active in a year
 attrition_rates_by_year_n <- clients %>%
   group_by(RC.Account.Number) %>%
   filter(year_n > 0, Year <= 2016) %>%
   arrange(year_n) %>%
   mutate(
        could_be_active_next_year = Year <= 2015 & active_year == TRUE,
        active_next_year = lead(active_year, 1) | lead(active_year, 2) ,  # check if active in next *two* years
        active_next_year = replace(active_next_year, is.na(active_next_year), FALSE)) %>%
   group_by(year_n) %>%
   # filter(active_year==TRUE) %>%
   summarise(
     active_clients = sum(active_year, na.rm = TRUE),
     could_be_active_next_year_sumum = sum(could_be_active_next_year, na.rm = TRUE),
     active_next_year_s = sum(active_next_year, na.rm = TRUE),
     attrition_rate = 1 - (sum(active_next_year, na.rm = TRUE) / sum(could_be_active_next_year, na.rm = TRUE)),
     survival_pct = active_clients / max(client_count_in_vintage_year, na.rm = TRUE),
     n = n())
  round(attrition_rates_by_year_n, 2)

  # running/cumulative survival
  attrition_rates_by_year_n$cumulative_survival <- NA
  attrition_rates_by_year_n$cumulative_survival[1] <- (1 - attrition_rates_by_year_n$attrition_rate[1])
  
  for (i in 2:nrow(attrition_rates_by_year_n)) {
    attrition_rates_by_year_n$cumulative_survival[i] <- (1 - attrition_rates_by_year_n$attrition_rate[i]) * attrition_rates_by_year_n$cumulative_survival[i - 1]
  }
  
  # plot attrition rates by year_n
  ggplot(attrition_rates_by_year_n, aes( x = year_n)) +
    geom_point(aes(y = attrition_rate)) +
    geom_line(aes(y = cumulative_survival)) +
    scale_y_continuous(labels = scales::percent)
  

# -----------------------------------------------------------------------------------------------------------
# --------------------------------survival rate 2006 vintage        -----------------------------------------
# -----------------------------------------------------------------------------------------------------------
  
  attrition_rates_2006 <- clients %>%
    filter(year_one == 2006, active_year == TRUE) %>%
    group_by(RC.Account.Number) %>%
    arrange(Year) %>%
     mutate(
          could_be_active_next_year = Year <= 2015 & active_year == TRUE, # 
          active_next_year = lead(active_year, 1) | lead(active_year, 2) ,  # check if active in next *two* years
          active_next_year = replace(active_next_year, is.na(active_next_year), FALSE)) %>%
     group_by(year_n) %>%
     # filter(active_year==TRUE) %>%
     summarise(
       active_clients = sum(active_year, na.rm = TRUE),
       could_be_active_next_year_sum = sum(could_be_active_next_year, na.rm = TRUE),
       active_next_year_s = sum(active_next_year, na.rm = TRUE),
       attrition_rate = 1 - (sum(active_next_year, na.rm = TRUE) / sum(could_be_active_next_year, na.rm = TRUE)),
       survival_pct = active_clients / max(client_count_in_vintage_year, na.rm = TRUE),
       n = n())
  
   attrition_rates_2006$cumulative_survival <- NA
  attrition_rates_2006$cumulative_survival[1] <- (1 - attrition_rates_2006$attrition_rate[1])
  
  for (i in 2:nrow(attrition_rates_2006)) {
    attrition_rates_2006$cumulative_survival[i] <- (1 - attrition_rates_2006$attrition_rate[i] ) * attrition_rates_2006$cumulative_survival[i - 1]
  }
  round(attrition_rates_2006, 2)
  
  # plot attrition rates by year_n
    survival_2006 <- ggplot(attrition_rates_2006, aes( x = year_n + 2005)) +
      # geom_point(aes(y = attrition_rate)) +
      geom_line(aes(y = cumulative_survival)) +
      scale_y_continuous('Survival Rate', labels = scales::percent) +
      scale_x_continuous('Year', labels = seq(2006, 2016, 2)) +
      geom_text(
        data = filter(attrition_rates_2006, year_n == 1 | year_n == 10),
        aes(y = cumulative_survival, label = paste0(active_clients, ' clients')), nudge_y = 0.02, nudge_x = 0.5, color = 'darkblue') + 
      theme_hc()
    survival_2006
  
    ggsave(paste0('survival_2006_vintage_', Sys.Date(),'.png'), plot = survival_2006, width = plot_width, height = plot_height)
  
    

# -----------------------------------------------------------------------------------------------------------
# --------------------------------  gazelle growth rates            -----------------------------------------
# -----------------------------------------------------------------------------------------------------------


# check clients with 'gazelle' growth rates
gazelle <- clients %>%
  group_by(RC.Account.Number) %>%
  arrange(Year) %>%
  mutate(
    five_year_cagr = ((lead(sales, 4) / sales)^1/4) - 1,
    three_year_cagr = (lead(sales, 2) / sales)^1/2 - 1) %>%
  filter(Year == year_one) %>%
  # distinct(RC.Account.Number, .keep_all = TRUE) %>%
  select(RC.Account.Number, three_year_cagr, five_year_cagr, year_one, years_active)
View(gazelle)

summary(gazelle$three_year_cagr)
summary(gazelle$five_year_cagr)


sum(gazelle$three_year_cagr > 0.20, na.rm = TRUE) 
sum(!is.na(gazelle$three_year_cagr))

# count above 20%
  sum(gazelle$three_year_cagr > 0.20, na.rm = TRUE) / sum(!is.na(gazelle$three_year_cagr), na.rm = TRUE)
  sum(gazelle$five_year_cagr > 0.20, na.rm = TRUE) / sum(!is.na(gazelle$five_year_cagr), na.rm = TRUE)
  sum(gazelle$three_year_cagr > 0.20, na.rm = TRUE)
  sum(gazelle$five_year_cagr > 0.20, na.rm = TRUE)
  sum(!is.na(gazelle$three_year_cagr))
  sum(!is.na(gazelle$five_year_cagr))

# count above 50%
  sum(gazelle$three_year_cagr > 0.50, na.rm = TRUE) / sum(!is.na(gazelle$three_year_cagr), na.rm = TRUE)
  sum(gazelle$five_year_cagr > 0.50, na.rm = TRUE) / sum(!is.na(gazelle$five_year_cagr), na.rm = TRUE)
  sum(gazelle$three_year_cagr > 0.50, na.rm = TRUE) 
  sum(gazelle$five_year_cagr > 0.50, na.rm = TRUE) 
  sum(!is.na(gazelle$three_year_cagr))
  sum(!is.na(gazelle$five_year_cagr))


gazelle %>% filter(five_year_cagr < 5) %>%
 ggplot() + geom_histogram(aes(x = five_year_cagr)) + scale_x_continuous(labels = scales::percent)

# -----------------------------------------------------------------------------------------------------------
# -------------------------------- write output                     -----------------------------------------
# -----------------------------------------------------------------------------------------------------------

file <- paste0('growth and attrition plot data ', Sys.Date(), '.xlsx')

# attrition
  write.xlsx(as.data.frame(attrition_rates_by_year_n), sheetName = 'attrition rates by year', file = file, row.names = FALSE)
  # need to convert from tbl_df, since write.xlsx expects a dataframe
# attrition 2006
  write.xlsx(as.data.frame(attrition_rates_2006), sheetName = 'attrition 2006', file = file, row.names = FALSE, append = TRUE)  
# plot data nominal growth
  write.xlsx(as.data.frame(client_nom_growth), sheetName = 'nominal growth', file = file, row.names = FALSE, append = TRUE)  
# plot data pct growth
  write.xlsx(as.data.frame(client_pct_growth), sheetName = 'percent growth', file = file, row.names = FALSE, append = TRUE)  
  
  
  
# best fit line
  
  
  
  