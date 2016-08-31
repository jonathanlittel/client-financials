############################
# PD model using Sales

library(caret)
library(dplyr)


#---------------------
# LOAD DATA
source(paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders ",
  "for RAP Modules/Risk Profile/PD Model/2.Model Selection/Preprocess data file.r",
  sep = ""))
  
#---------------------
# TRAIN MODEL

modelCols <- c('Sales_log', 'Tenor_years', 'WO')

df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols]

tc <- trainControl("repeatedcv",
                   number=5,
                   repeats=10, 
                   savePred=TRUE,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)
# train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
set.seed(1348)
glm.sales <- train(WO ~ .  ,
                     data = df.train.model,
                     method='glm',
                     trControl=tc,
                     metric = "ROC",
                     family='binomial') 

# df.rap.inactive$pd <- predict(glm.sales, df.rap.inactive, type="prob")[,2]
#   
#   pd <- predict(glm.sales, df.rap.inactive, type="prob")[,2]
#   roc_all <- roc(df.rap.inactive$WO, pd)
#   df.rap.inactive$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
#   confusionMatrix(df.rap.inactive$predWO_cut, df.rap.inactive$WO)
#   summary(glm.sales$finalModel)
#   glm.sales$results$ROC
  