
library(ROCR)
library(pROC)

#######################
# HELPER FUNCTIONS
#######################

# AUC
auc <- function(model = logModel, 
                testdata = donTEST,
                testresponse = donTEST$donated){
  
  p <- predict(model, newdata=testdata, type="response")
  pr <- prediction(p, testresponse)
  prf <- performance(pr, measure = "auc")
  auc <- prf@y.values[[1]]
  
  return(auc)
}

# CCR
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
