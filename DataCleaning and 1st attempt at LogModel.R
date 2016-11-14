library(dplyr)

##############################################
#READ DATA INTO R
##############################################

#Set appropriate file address
#Kristin
address <- ''
#Adit
address <- ''
#Dylan
address <- 'C:/Users/Dylan/Documents/Northwestern/Predictive Analytics I/PROJECT/'
#Dustin
address <- 'C:/Users/Dustin/Documents/Northwestern/Predictive Analytics 1/PredictiveProjectADDK/'

setwd(address)

#read files
donData <- read.table("Description and Raw Data/donation data.csv", sep = ',',header = TRUE)
CodeCatTable <- read.table("Description and Raw Data/dmef1code.csv", sep = ',',header = TRUE)
stateLookup <- read.table("Description and Raw Data/state_table.csv", sep = ',',header = TRUE)


##############################################
#CLEAN DATA
##############################################

# Where previous donations do not exist, replace NA with 0
donData$CNDOL2[is.na(donData$CNDOL2)] <- 0
donData$CNDOL3[is.na(donData$CNDOL3)] <- 0

# Lookup category of each contribution type and solicitation typ 
donData <- donData %>% left_join(CodeCatTable, by = c('CNCOD1'='CODE')) %>% rename(ContType1 = CODETYPE) %>%
                 left_join(CodeCatTable, by = c('CNCOD2'='CODE')) %>% rename(ContType2 = CODETYPE) %>%
                 left_join(CodeCatTable, by = c('CNCOD3'='CODE')) %>% rename(ContType3 = CODETYPE) %>%
                 left_join(CodeCatTable, by = c('SLCOD1'='CODE')) %>% rename(SolType1 = CODETYPE) %>%
                 left_join(CodeCatTable, by = c('SLCOD2'='CODE')) %>% rename(SolType2= CODETYPE) %>%
                 left_join(CodeCatTable, by = c('SLCOD3'='CODE')) %>% rename(SolType3 = CODETYPE)

#donData$ContType1[is.na(donData$ContType1)==T] <- '*'
#donData$ContType2[is.na(donData$ContType2)==T] <- '*'
#donData$ContType3[is.na(donData$ContType3)==T] <- '*'
#donData$SolType1[is.na(donData$SolType1)==T] <- '*'
#donData$SolType2[is.na(donData$SolType2)==T] <- '*'
#donData$SolType3[is.na(donData$SolType3)==T] <- '*'

##############################################
#ADD NEW POTENTIALLY USEFUL COLUMNS TO DATA
##############################################

# Get region of each state
donData <- donData %>% left_join(stateLookup, by = c('STATCODE'='abbreviation'))

# get average of all donations
donData$avg <- donData$CNTRLIF / donData$CNTMLIF

# takes average of last 3 donations
#donData$avgLast3 <- rowMeans(cbind(donData$CNDOL1,donData$CNDOL2,donData$CNDOL3), na.rm = T)
# not using currently because it was linearly dependant

# get average time between donations if multiple exist, otherwise NA
donData$avgTime <- with(donData, ifelse(is.finite((CNMONF - CNMON1)/ (CNTMLIF-1)),(CNMONF - CNMON1)/ (CNTMLIF-1) ,0)   )
#donData$avgTime[donData$avgTime==0] <- NA


##############################################
#ADD RESPONSE VARIABLE
##############################################
# response variable for Logisitic Model
donData$donated <- ifelse(donData$TARGDOL>0,1,0)


##############################################
#LOGISTIC REGRESSION MODEL
##############################################
# Split into TRAINING and TEST sets
# Remove some columns
TESTindices <- seq(from = 3,to = nrow(donData), by = 3)
donTRAINING <- subset( donData, select=-c(STATCODE,TARGDOL,ID,CNDAT1,CNDAT2,CNDAT3,CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )
donTEST <- subset( donData, select=-c(STATCODE, TARGDOL,ID,CNDAT1,CNDAT2,CNDAT3, CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )

#fit basic logistic regression model
logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)
summary(logModel)

#install.packages('ROCR')
library(ROCR)
# make predictions on TEST set then give AUC
p <- predict(logModel, newdata=donTEST, type="response")
pr <- prediction(p, donTEST$donated)
prf <- performance(pr, measure = "auc")
prf@y.values[[1]]
# AUC 0.7725

# confusion matrix
table(donTEST$donated,p>0.5)

#install.packages('caret')
library(caret)
# idk exactly what this calculates, but most the variables deemed most important seem to make sense
x<-varImp(logModel, scale = FALSE)
x$variableName <- rownames(x)
x[with(x,order(-Overall)),]

##############################################
# Second Order Model with some interactions included, attempting to maximize AUC
##############################################


logModel2 <- glm(donated ~ . + (CNMONL+CNTMLIF+CNMONF+CNMON3+CNMON1+CNDOL1+ContType1+CONLARG+CNTRLIF)^2, data = donTRAINING, family=binomial)

p2 <- predict(logModel2, newdata=donTEST, type="response")
pr2 <- prediction(p2, donTEST$donated)
prf2 <- performance(pr2, measure = "auc")
prf2@y.values[[1]]
# AUC 0.7902888

##############################################
#fit multiple regression model for predicting donation amount
##############################################

#split into training and test set again
donTRAINING2 <- subset( donData, select=-c(STATCODE,ID,CNDAT1,CNDAT2,CNDAT3,CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )
donTEST2 <- subset( donData, select=-c(STATCODE,ID,CNDAT1,CNDAT2,CNDAT3, CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )

#filter data to only people that donated
donTRAINING2 <- donTRAINING2[donTRAINING2$TARGDOL > 0,]

#fit multiple regression model
mrModel <- lm(TARGDOL ~ ., data=donTRAINING2)
summary(mrModel)

##############################################
#Calculate expected donation for each person
##############################################

#get probability that each person donates from logistic model
donData$prob <- predict.glm(logModel2, newdata=donData, type="response")

#get guess of each person's donation from multiple regression model
donData$donGuess <- predict.lm(mrModel, newdata=donData, na.action = na.pass)
donData$donGuess <- ifelse(donData$donGuess < 0, 0, donData$donGuess)

#get expected value of each person's donation
donData$expDon <- donData$prob * donData$donGuess
summary(donData$expDon)






##############################################
#SCRATCH WORK
##############################################
#add second degree terms to dataframe
z <- 0
numVars <- length(names(donTRAINING))-1
for (i in 1:numVars) {
  for (j in 1:i) {
      z <- z + 1
      assign(paste("col", z, sep = '_'), donTRAINING[,i]*donTRAINING[,j])
      donTRAINING <- cbind(donTRAINING,  eval(as.name(paste('col_',z,sep=''))))
      assign(paste("col2", z, sep = '_'), donTEST[,i]*donTEST[,j])
      donTEST <- cbind(donTEST,  eval(as.name(paste('col2_',z,sep=''))))
  }
}
names(donTRAINING)[24:276] <- seq(24,276,by=1)
names(donTEST)[24:276] <- seq(24,276,by=1)

#remove columns with 1 level
for (x in names(donTRAINING)) {
  if (length(unique(donTRAINING[,x]))==1) {
    donTRAINING[,x] <- NULL
    donTEST[,x] <- NULL
  }
}

logModel2 <- glm(donated ~ . , data = donTRAINING)

library(ROCR)
# make predictions on TEST set then give AUC
p <- predict(logModel2, newdata=donTEST, type="response")
pr <- prediction(p, donTEST$donated)
prf <- performance(pr, measure = "auc")
prf@y.values[[1]]



################################ Unsuccessful attempt at full second order model ###########

# 
# # create 2nd order data sets
# donTRAININGresponse <- donTRAINING$donated
# donTESTresponse <- donTEST$donated
# donTRAINING2 <- data.frame(model.matrix(donated~.^2, model.frame(~.,donTRAINING,na.action='na.pass')) )
# donTEST2 <- data.frame(model.matrix(donated~.^2,model.frame(~.,donTEST,na.action='na.pass')))
# donTRAINING2 <- cbind(donTRAINING2, donTRAININGresponse)
# donTEST2 <- cbind(donTEST2, donTESTresponse)
# 
# # renaming crap so it will work
# names(donTEST2) <- names(donTRAINING2)
# 
# library(ROCR)
# # fit 2nd order model
# logModelAAA <- glm(donTRAININGresponse~., family ='binomial', data=donTRAINING2)
# pAAA <- predict(logModelAAA, newdata=donTEST2, type="response")
# prAAA <- prediction(pAAA, donTEST2$donTRAININGresponse)
# prfAAA <- performance(prAAA, measure = "auc")
# prfAAA@y.values[[1]]
# # AUC not any better
# dim(donTEST2$donTESTresponse)
# 
# table(donTEST$donTESTresponse,pAAA>0.5)
