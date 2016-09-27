
scott <- bal2 %>% group_by(RC.Account.Number, date) %>% 
  summarise(
    balance = sum(balance, na.rm = TRUE),
    balance_max = max(balance, na.rm = TRUE) ) %>%
  ungroup() %>%
  mutate(Year = year(date)) %>%
  group_by(RC.Account.Number, Year) %>%
  summarise(
    balance = max(balance_max, na.rm = TRUE))


scott$hj <- rep(c(1,0,-1), length.out= 18 *2)
scott$ypos[scott$balance <= 5e5]<- -15
scott$ypos[scott$balance > 5e5]<- -55
# http://stackoverflow.com/questions/16127170/how-to-prevent-two-labels-to-overlap-in-a-barchart

clients500k.bar <- ggplot(filter(scott, Year>2007), aes(x = Year, fill = balance > 5e5)) + 
  geom_histogram(bins = 9, alpha = .85, position = "stack") +                                    # 18 bins if all years
  # stat_bin(aes(y=..count.., label=..count..,  colour=ifelse(scott$balance > 5e5,"#F8766D","#00BFC4")), binwidth = 1, geom="text", vjust=-1.5,  bins = 18) +
  stat_bin(aes(y=..count.., label=..count..), binwidth = 1, geom="text", vjust=1.5,  bins = 9) + # 18 bins if all years
  # geom_text(data )
  theme_minimal() + scale_fill_brewer(palette = 12)



scott <- arrange(scott, balance > 5e5)
scott <- scott %>% group_by(RC.Account.Number) %>% 
  mutate(year_one = min(Year))

ggplot(scott, aes(x = Year, y = balance, fill = balance > 5e5)) + 
  geom_bar(alpha = .85, stat = 'identity', position = "stack" ) +
  # geom_bar(alpha = .95, stat = 'identity', position = position_dodge(width=0.9), color = 'purple') +
  scale_y_continuous('Peak client balance', labels = scales::dollar) +
  theme_minimal() + scale_fill_brewer()    

ggplot(scott, aes(x = Year, y = balance, fill = balance > 5e5)) + 
  geom_bar(alpha = .85, stat = 'identity', position = "stack" ) +
  # geom_bar(alpha = .95, stat = 'identity', position = position_dodge(width=0.9), color = 'purple') +
  scale_y_continuous('Peak client balance', labels = scales::dollar) +
  theme_minimal() + scale_fill_brewer()    


scott <- arrange(scott, Year==year_one)
ggplot(filter(scott, balance < 5e5), aes(x = Year, y = balance, fill = Year==year_one)) + 
  geom_bar(alpha = .85, stat = 'identity', position = "stack" ) +
  # geom_bar(alpha = .95, stat = 'identity', position = position_dodge(width=0.9), color = 'purple') +
  scale_y_continuous('Peak client balance', labels = scales::dollar) +
  theme_minimal() + scale_fill_brewer()    



g = ggplot(unrate.df) + geom_line(aes(x=date, y=UNRATE)) + theme_bw()
g = g + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.2)