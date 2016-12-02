
library(ROCR)
library(pROC)

#######################
# HELPER FUNCTIONS
#######################

#######################
# AUC
#######################

auc <- function(model = logModel, 
                testdata = donTEST,
                testresponse = donTEST$donated){
  
  p <- predict(model, newdata=testdata, type="response")
  pr <- prediction(p, testresponse)
  prf <- performance(pr, measure = "auc")
  auc <- prf@y.values[[1]]
  
  return(auc)
}


#######################
# CCR
####################### 

ccr <- function(model = logModel, 
                testdata = donTEST,
                testresponse = donTEST$donated,
                pstar=.5){
  # confusion matrix
  p <- predict(model, newdata=testdata, type="response")
  tab = table(testresponse, p > pstar)
  CCR = sum(diag(tab))/sum(tab)
  
  return(CCR)
}

#######################
# Expected Donations (top 1000)
#######################

expected.don <- function(final.log.model = logModel,
                         final.lm.model = lmModel,
                         final.data = donTEST_orig){
  #get probability that each person donates from logistic model
  final.data$prob <- predict.glm(final.log.model, newdata=final.data, type="response")
  
  #get guess of each person's donation from multiple regression model
  final.data$donGuess <- predict.lm(final.lm.model, newdata=final.data, na.action = na.pass)
  final.data$donGuess <- ifelse(final.data$donGuess < 0, 0, final.data$donGuess)
  
  #get expected value of each person's donation
  final.data$expDon <- final.data$prob * final.data$donGuess
  sum.don <- summary(final.data$expDon)
  
  #Select 1000 donors from the test set who have the highest E(TARGDOL). These may be the donors
  #that will be special marketing targets. Then find their total actual donations. This is the payoff
  #and should be as high as possible.
  top1000 <- final.data[order(final.data$expDon, decreasing=T),][1:1000,]
  tot.don <- sum(top1000$targdol) 
  
  return(tot.don)
}