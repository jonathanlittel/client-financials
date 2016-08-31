library(dplyr)  # for data manipulation
library(readxl) # for read_excel

#------------ READ DATA -----------------------------------
# set working directory
  wd <- 'c:/Box Sync/jlittel/comms/client financials/data'
  setwd(wd)
# read first data
  # may need to first format everything to 'general' in excel if there are commas or oddities from excel
  filename <- 'SEMs for all Clients.xlsx'
  sems_raw <- read_excel(filename, sheet = 1, na = "NA")
  glimpse(sems_raw) # examine

# read second data
  filename2 <- 'alex_data_formatted.csv'
  sems_alex <- read.csv(filename2)
  glimpse(sems_alex)

#------------ PREP DATA -----------------------------------

  sum(duplicated(sems_raw[,c(4,6)]))
  sems_raw$from_faina <- TRUE
  sems_alex$from_alex <- TRUE
  # set the names of the id fields to be the same for the merge
  sems_raw <- rename(sems_raw, Data_Year = `Data Year `)
  # sems_raw <- rename(sems_raw, temp_name = `Account Name`, Data_Year = `Data Year `)
  # sems_alex <- rename(sems_alex, temp_name = client, Data_Year = year)
  sems_alex <- rename(sems_alex, Data_Year = year)
  sems_raw$Account_Name <- tolower(sems_raw$`Account Name`)
  sems_alex$Account_Name <- tolower(sems_alex$client)
  sems_raw$temp_name <- NULL
  sems_alex$temp_name <- NULL
 
#------------ MERGE ---------------------------------------
  # merge in RC.Account.Number to sems_alex
  # note that id_match came from elsewhere: 
  #   > names(id_match)
  # [1] "RC.Account.Number" "Account.Name"   
  id_match$Account_Name <- tolower(id_match$Account.Name)
  sems_alex_id <- merge(sems_alex, id_match, by = 'Account_Name', all.x = TRUE)
  sems_joined <- merge(x = sems_raw, y = sems_alex_id, by = c('Account_Name', 'Data_Year'), all = TRUE)


#------------ REMOVE ROWS THAT ARE ALL NA FROM COL----------------

# remove rows that are all NA from col 3 onward
# all columns except Account_Name, Data_Year, from_faina, and from_alex
blank_rows <- apply(sems_joined[c(3:37, 39:63)], 1, function(x) all(is.na(x)))
sems_out <- sems_joined[!blank_rows, ]

#------------ SAVE ----------------------------------------
# turn this on to save:
write.csv(sems_joined, 'master_data_with_alex.csv', row.names = FALSE)