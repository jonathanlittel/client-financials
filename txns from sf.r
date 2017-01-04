library(RForcecom)

## OK - let's log in - from the package documentation 
username <- "jlittel@rootcapital.org" # normal email/sflogin
password <- "=nqxEVnkr4qBpP3>ZRpfUpL7YWs5g0B2unQOQfOu3Hmp" # this is a combination of your password and app token
# password <- "Y]i}aT4%ku8LH[tQ.Wy73avedUBRZTkkXstiBPMMoOeT" # this is a combination of your password and app token
instanceURL <- "https://rootcapital.my.salesforce.com/"
# instanceURL <- "https://na3.salesforce.com/"
apiVersion <- "36.0"  ## may change as api version changes
session <- rforcecom.login(username, password, instanceURL, apiVersion)

# run transaction report
# note that transaction amounts with record type of disbursement are before taking in to account fees
# there is a separate field for the fee amount, and the outgoing cash amount
soqlQuery1 <- paste("SELECT Opportunity__r.RC_Opp_Number__c, Opportunity__r.Account.Country__c, 
  RecordType.Name, Date__c, Internal_Transaction_Amount__c FROM Transaction__c ",
  "WHERE Opportunity__r.Portfolio__c!='LAFCo' AND (RecordTypeId = '01270000000E136AAC' OR ",
  "RecordTypeId = '01270000000E57JAAS' OR RecordTypeId = '01270000000E57YAAS' OR ",
  "RecordTypeId = '01270000000E57dAAC' OR RecordTypeId = '01270000000E57OAAS' OR RecordTypeId = '01270000000E13DAAS')",
  sep = "")

# attempt to read loan id to match to opp number
# test <- rforcecom.query(session, "SELECT Opportunity__r.RC_Opp_Number__c, Default_Loan_Closing_ID__c FROM Transaction__c WHERE RecordTypeId = '01270000000E137' AND Opportunity__r.Portfolio__c!='LAFCo'")

# Actual Repayments
soqlQuery2 <- "SELECT Opportunity__r.RC_Opp_Number__c, Opportunity__r.Account.Country__c, 
RecordType.Name, Internal_Date__c, Internal_Transaction_Amount__c, Internal_Principal_Paid__c, 
Internal_Interest_Paid__c, Internal_Penalty_Interest_Paid__c, 
Internal_Recoveries_Paid__c FROM Transaction__c WHERE RecordTypeId = '01270000000E137' AND Opportunity__r.Portfolio__c!='LAFCo'"

# run queries
txns <- rforcecom.query(session, soqlQuery1)
rpts <- rforcecom.query(session, soqlQuery2)

# save to avoid re-running query
txns_backup <- txns
rpts_backup <- rpts
# convert to numeric
num_cols <- names(txns)[c(2)]
txns[num_cols] <- sapply(txns[num_cols], function(x) as.numeric(as.character(x)))
num_cols <- names(rpts)[c(2:5,11)]
rpts[num_cols] <- sapply(rpts[num_cols], function(x) as.numeric(as.character(x)))

txns$Date <- as.Date(as.character(txns$Date__c, '%y-%m-%d'))

ds <- txns %>%
  group_by(Opportunity.RC_Opp_Number__c) %>%
  filter(RecordType.Name == 'Disbursement') %>%   # note that there are six transaction types, adjustments
  summarise(
    disb    = sum(Internal_Transaction_Amount__c , na.rm = TRUE),  # for some reason this produces a matrix. probably a two field object (currency type and #)
    disb_n  = n(),
    year_origination = year(min(Date))
  )



levels(txns$RecordType.Name)                      # note that there are six transaction types, adjustments

apply(ds[,2:3], 2,  mean)
apply(ds[,2], 2,  sum)

rp <- rpts %>%
  group_by(Opportunity.RC_Opp_Number__c) %>%
  summarise(
    pmt_pr  = sum(Internal_Principal_Paid__c, na.rm = TRUE),  # not sure what this is..
    pmt_int = sum(Internal_Interest_Paid__c + Internal_Penalty_Interest_Paid__c, na.rm = TRUE),    
    pmt_rec = sum(Internal_Recoveries_Paid__c, na.rm = TRUE),
    pmt     = pmt_pr + pmt_int
  )  

apply(rp[,2:5], 2, sum)

tx <- merge(ds, rp, by = 'Opportunity.RC_Opp_Number__c', all = TRUE)

tx <- rename(tx, RC.Opp.Number = Opportunity.RC_Opp_Number__c)
tx$RC.Opp.Number <- as.numeric(as.character(tx$RC.Opp.Number))

# # no idea why this is necessary, but
class(tx$disb) # ?? matrix??
temp_disb <- as.vector(tx$disb)
tx$disb <- temp_disb
class(tx$disb)

tx2 <- tx %>% mutate(revenue = pmt - disb, yield = pmt / disb)

write.csv(tx2, 'tx2.csv', row.names = F)
