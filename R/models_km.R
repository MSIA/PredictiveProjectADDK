
#install.packages('ROCR')
library(ROCR)
library(pROC)

who = "kristin"

# Set appropriate file address
if(who=="kristin"){
  address <- '/Users/kmeier92/Documents/Northwestern/fall2016/Predictive_Analytics/PredictiveProjectADDK/'
}
if(who=="adit"){
  address <- ''
}
if(who=="dylan"){
  address <- 'C:/Users/Dylan/Documents/Northwestern/Predictive Analytics I/PROJECT/'
}
if(who=="dustin"){
  address <- 'C:/Users/Dustin/Documents/Northwestern/Predictive Analytics 1/PredictiveProjectADDK/'
}

# source R code that cleans the data as needed
source(paste(address,'R/cleandata.R',sep=""))
# functions auc & ccr
source(paste(address,'R/helper.R',sep=""))

# call function to create the 2 datasets 
thedata <- cleandata(dropNA=T)

donTRAINING_orig <- thedata$train
donTEST_orig <- thedata$test

##############################################
# LOGISTIC REGRESSION MODEL
##############################################
# fit basic logistic regression model
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Split into TRAINING and TEST sets
# Remove some columns
# "CNDOL1"    "CNTRLIF"   "CONLARG"   "CONTRFST"  "CNCOD1"    "CNCOD2"    "CNCOD3"    "CNDAT1"   
# "CNDAT2"    "CNDAT3"    "CNDOL2"    "CNDOL3"    "CNTMLIF"   "SLCOD1"    "SLCOD2"    "SLCOD3"   
# "TARGDOL"   "STATCODE"  "SEX"       "CNMON1"    "CNMON2"    "CNMON3"    "CNMONF"    "CNMONL"   
# "ID"        "ContType1" "ContType2" "ContType3" "SolType1"  "SolType2"  "SolType3"  "Region"   
# "avg"       "avgTime"   "don2"      "don3"      "donated" 

# QUAD TERMS
# "sq_CNDOL1"   "sq_CNTRLIF"  "sq_CONLARG"  "sq_CONTRFST" "sq_CNCOD1"   "sq_CNCOD2"   "sq_CNCOD3"   "sq_CNDAT1"  
# "sq_CNDAT2"   "sq_CNDAT3"   "sq_CNDOL2"   "sq_CNDOL3"   "sq_CNTMLIF"  "sq_SLCOD1"   "sq_SLCOD2"   "sq_SLCOD3"  
# "sq_CNMON1"   "sq_CNMON2"   "sq_CNMON3"   "sq_CNMONF"   "sq_CNMONL"   "sq_avg"      "sq_avgTime"  "sq_don2"    
# "sq_don3"   

dropcols <- c("STATCODE","TARGDOL","ID","CNDAT2","CNDAT3","CNCOD1","CNCOD2","CNCOD3"," SLCOD1","
             SLCOD2","SLCOD3"," CNMON2"," CNMON3"," SolType2"," SolType3","ContType2","ContType3","Region")

# what to do regression on
keepcols <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", "
                CNDOL3", "CNTMLIF", "SLCOD1", "SLCOD2", "SEX", "CNMON1", "CNMON2", "
                CNMON3", "CNMONF", "CNMONL", "ContType1", "SolType1", "SolType2", "
                SolType3", "avg", "avgTime", "don2", "don3")

keepcols <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", "
                CNDOL3", "CNTMLIF", "SLCOD1", "SLCOD2", "SEX", "CNMON1", "CNMON2", "
              CNMON3", "CNMONF", "CNMONL", "ContType1", "SolType1", "SolType2", "
              SolType3", "avg", "avgTime", "don2", "don3","CNDOL3_don3","CNDOL2_don2")

## you can either have: DATES OF CONTRIBUTION or MONTHS SINCE LATEST CONTRIBUTION
## since all data is using 1 date as current date. these are redundant 

donTRAINING <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols)]
donTEST <- donTEST_orig[,(names(donTEST_orig) %in% keepcols)]

### BASICALLY WE WANT TO KEEP TRACK OF THE VARIABLES WE USED
### WHAT THE AUC IS 
### CORRECT RATE
### # SIG PREDICTORS
### # TOTAL PREDICTORS
### 
### ASSUME WE ARE USING THIS AS A MEASURE OF 'BEST MODEL'

logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)
sum.mod <- summary(logModel)

num.pred <- nrow(sum.mod$coefficients)

auc(model=logModel)
ccr(model=logModel)

##########################
# BACKWARDS STEPWISE
##########################
logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)

backwards = step(logModel)
summary(backwards)

auc(model=backwards)
ccr(model=backwards)

# start
'
donated ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNDAT1 + CNDOL2 + 
  CNDOL3 + CNTMLIF + SLCOD1 + SLCOD2 + SEX + CNMON1 + CNMON2 + 
  CNMON3 + CNMONF + CNMONL + ContType1 + SolType1 + SolType2 + 
  SolType3 + avg + avgTime + don2 + don3
'

# final
'
donated ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNDAT1 + CNDOL2 + 
  CNTMLIF + SLCOD2 + SEX + CNMON2 + CNMON3 + CNMONF + CNMONL + 
  ContType1 + SolType1 + SolType3 + avg + avgTime
'

##########################
# FORWARD STEPWISE
##########################

nothing <- glm(donated ~ 1, data = donTRAINING, family=binomial)
forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(logModel)),
                direction="forward")

summary(forwards)

# final
'
donated ~ CNMON2 + CNMONL + CNDAT1 + CNMON3 + ContType1 + CONTRFST + 
    SolType3 + CNTMLIF + CNMONF + CNDOL1 + CONLARG + SLCOD2 + 
CNTRLIF + CNDOL2 + avg + avgTime + SolType1 + SEX
'
auc(model=forwards)
ccr(model=forwards)

######################################################
# DO FORWARD AND BACKWARD W/ ALL QUADRATIC AND INTERACTION TERMS AS OPTIONS...
# MAY TAKE A WHILE BUT WHY NOT?
#########################################################################

# FULL MODEL CURRENTLY
'donated ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNDAT1 + CNDOL2 + 
  CNDOL3 + CNTMLIF + SLCOD1 + SLCOD2 + SEX + CNMON1 + CNMON2 + 
  CNMON3 + CNMONF + CNMONL + ContType1 + SolType1 + SolType2 + 
  SolType3 + avg + avgTime + don2 + don3
'


####################################################
# STOP HERE FOR NOW...ADDED QUADRATIC AND INTERACTION TERMS TO THE CLEANDATA.R


logModel2 <- glm(donated ~ . + (CNMONL+CNTMLIF+CNMONF+CNMON1+CNDOL1+ContType1+CONLARG+CNTRLIF+SolType1)^2, data = donTRAINING, family=binomial)


#install.packages('caret')
library(caret)
# idk exactly what this calculates, but most the variables deemed most important seem to make sense
x<-varImp(logModel, scale = FALSE)
x$variableName <- rownames(x)
x[with(x,order(-Overall)),]

##############################################
# Second Order Model with some interactions included, attempting to maximize AUC
##############################################

logModel2 <- glm(donated ~ . + (CNMONL+CNTMLIF+CNMONF+CNMON1+CNDOL1+ContType1+CONLARG+CNTRLIF+SolType1)^2, data = donTRAINING, family=binomial)

p2 <- predict(logModel2, newdata=donTEST, type="response")
pr2 <- prediction(p2, donTEST$donated)
prf2 <- performance(pr2, measure = "auc")
prf2@y.values[[1]]
# AUC 0.722847

##############################################
#fit multiple regression model for predicting donation amount
##############################################

#split into training and test set again
donSET2 <- subset( donData, select=-c(STATCODE,ID,CNDAT1,CNDAT2,CNDAT3,CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3, CNMON2, CNMON3, SolType2, SolType3,ContType2,ContType3) )
donTRAINING2 <- donSET2[-TESTindices,]
donTEST2 <- donSET2[TESTindices,]


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
