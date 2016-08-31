# library(xlsx)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr) # for str_split_fixed
  wd_data <- 'C:/Box Sync/jlittel/comms/client financials/data'
  setwd(wd_data)
  # master_data_file <- 'https://rootcapital.box.com/shared/static/8ewpklalcl8bc9pitcnnformm2edjgnz.xlsx'
  master_data_file <- 'Master Database.xlsx'
  df <- read_excel(master_data_file, sheet = 'Metrics')

# transpose dataframe, rename columns, remove first row (was column names) and format to numeric)
	  df.alex <- t(df)
	  colnames(df.alex) <- df.alex[1,]
	  df.alex <- df.alex[-1,]
	  client_names <- rownames(df.alex)
	  df.alex <- as.data.frame(df.alex)
	  df.alex <- df.alex %>% select(noquote(order(colnames(df.alex)))) # sort alphabetically
	  df.alex$client <- client_names
	  df.alex[,400] <- NULL           # remove a blank column

# gather all to three columns
	 df.long <- gather(df.alex, key = account_year, value = value, -client)
	 # space_count <- str_count(df.long$account_year, "_")
	 # split_years <- str_split_fixed(df.long$account_year, '_', 3)
	 df.long$year <- str_sub(df.long$account_year, start= -4)        # get last four chars for year
	 df.long$account <- str_sub(df.long$account_year, end= -6)       # get account name without year
	 # colnames(split_years) <- c('account1', 'account2', 'account3')
	 # df.long <- cbind(df.long, split_years)
     df.long$account_year <- NULL

# format numbers
     df.long$value <- as.numeric(as.character(df.long$value))
     df.long$year <- as.numeric(as.character(df.long$year))

# spread back accounts to column headers
     # df.long$row <- 1:nrow(df.long)  # add identifier 
     df.long.filt <- df.long %>% distinct( account, year, client, .keep_all = TRUE)
	 df.good <- spread(df.long.filt, key = account, value = value)
	 # df.good$row <- NULL

# count duplicate client names
	 client_names[duplicated(client_names)]
# write output
	 write.csv(df.good, 'alex_data_formatted.csv', row.names = FALSE)