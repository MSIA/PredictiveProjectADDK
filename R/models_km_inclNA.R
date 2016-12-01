
#install.packages('ROCR')
library(ROCR)
library(pROC)

who = "adit"

# Set appropriate file address
if(who=="kristin"){
  address <- '/Users/kmeier92/Documents/Northwestern/fall2016/Predictive_Analytics/PredictiveProjectADDK/'
}
if(who=="adit"){
  address <- "/Users/arvenkat/Documents/PredictiveProjectADDK/"
}
if(who=="dylan"){
  address <- 'C:/Users/Dylan/Documents/Northwestern/Predictive Analytics I/PROJECT/'
}
if(who=="dustin"){
  address <- 'C:/Users/Dustin/Documents/Northwestern/Predictive Analytics 1/PredictiveProjectADDK/'
}

# source R code that cleans the data as needed
setwd(address)
source(paste(address,'R/cleandata.R',sep=""))
# functions auc & ccr
source(paste(address,'R/helper.R',sep=""))

# call function to create the 2 datasets 
thedata <- cleandata(dropNA=F)

donTRAINING_orig <- thedata$train
donTEST_orig <- thedata$test

#add quadratic and interaction terms to data
donTRAINING_orig <- addSecondDegree(donTRAINING_orig)
donTEST_orig <- addSecondDegree(donTEST_orig)

#####################################################
#####################################################
######################################################
# LOGISTIC REGRESSION MODEL
######################################################
#####################################################
#####################################################

# fit basic logistic regression model
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

  # what to do regression on
  keepcols <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST",
                "CNDOL2", "CNDOL3", "CNTMLIF", "SEX", "CNMON1", 
                 "CNMONF", "CNMONL", "ContType1", "SolType1",
                 "avg", "avgTime", "don2", "don3","incr_don", "Region")
  
  #filter data sets to appropriate columns
  donTRAINING <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols)]
  donTEST <- donTEST_orig[,(names(donTEST_orig) %in% keepcols)]
  
  ### WHAT THE AUC IS 
  ### CORRECT RATE
  ### # SIG PREDICTORS
  ### # TOTAL PREDICTORS
  ### ASSUME WE ARE USING THIS AS A MEASURE OF 'BEST MODEL'
  
  logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)
  sum.mod <- summary(logModel)
  
  num.pred <- nrow(sum.mod$coefficients)
  
  auc(model=logModel) # 0.716036
  ccr(model=logModel) # 0.7530394

##########################
# BACKWARDS STEPWISE
##########################

  logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)
  
  backwards = step(logModel)
  summary(backwards)
  
  auc(model=backwards) # 0.716004
  ccr(model=backwards) # 0.7531906
  
  # final with NA
  'donated ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + CNDOL2 + CNTMLIF + 
    CNMON1 + CNMONF + CNMONL + avgTime + don2 + don3 + incr_don + 
    SEX + ContType1 + SolType1 + Region'

##########################
# FORWARD STEPWISE
##########################
  
  nothing <- glm(donated ~ 1, data = donTRAINING, family=binomial)
  forwards = step(nothing,
                  scope=list(lower=formula(nothing),upper=formula(logModel)),
                  direction="forward")
  
  summary(forwards)
  
  # final w/ NA
  '
  donated ~ CNMON1 + CNMONL + CNTMLIF + CNMONF + ContType1 + CONTRFST + 
    SolType1 + incr_don + SEX + don2 + avgTime + CNTRLIF + CNDOL2 + 
    CNDOL1 + CONLARG + don3 + Region
  '
  
  auc(model=forwards) # 0.716004
  ccr(model=forwards) # 0.7531906


######################################################
# QUADRATIC TERMS ADDED
# http://stats.stackexchange.com/questions/28730/does-it-make-sense-to-add-a-quadratic-term-but-not-the-linear-term-to-a-model
######################################################


# USE OUTPUT FROM STEPWISE REGRESSION + SQUARE TERMS.

#"donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST",
#"CNDOL2", "CNDOL3", "CNTMLIF", "SEX", "CNMON1", 
#"CNMONF", "CNMONL", "ContType1", "SolType1",
#"avg", "avgTime", "don2", "don3","incr_don"

  
  keepcols.quad <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST",  "CNDOL2", "CNDOL3",
              "CNTMLIF", "SEX", "CNMON1","CNMONF", "CNMONL", "ContType1", 
              "SolType1",  "avg", "avgTime", "don2", "don3", "incr_don", "Region", "sq_CNDOL1", 
              "sq_CNTRLIF", "sq_CONLARG", "sq_CONTRFST",
               "sq_CNDOL2", "sq_CNDOL3", "sq_CNTMLIF", "sq_CNMON1", 
              "sq_CNMONF", "sq_CNMONL", "sq_avg", "sq_avgTime")

  donTRAINING.quad <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.quad)]
  donTEST.quad <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.quad)]
  
  logModel.quad <- glm(donated ~ . , data = donTRAINING.quad, family=binomial)
  sum.mod.quad <- summary(logModel.quad)
  
  num.pred.quad <- nrow(sum.mod.quad$coefficients)
  
  auc(model=logModel.quad, testdata = donTEST.quad) # 0.7194766
  ccr(model=logModel.quad, testdata = donTEST.quad) # 0.7550656

###################################
# BACKWARDS STEPWISE - W/ QUADRATIC
###################################

  logModel.quad <- glm(donated ~ . , data = donTRAINING.quad, family=binomial)
  
  backwards.quad = backwards
  
  backwards.quad = step(logModel.quad)
  summary(backwards.quad)
    
  'final w/ NA
  donated ~ sq_CNDOL1 + sq_CONLARG + sq_CNDOL2 + sq_CNTMLIF + sq_CNMON1 + 
    sq_CNMONL + sq_avg + sq_avgTime + CNDOL1 + CNTRLIF + CONLARG + 
    CONTRFST + CNDOL2 + CNTMLIF + CNMON1 + CNMONF + CNMONL + 
    avg + avgTime + don2 + don3 + incr_don + SEX + ContType1 + 
    SolType1 + Region
  '

  auc(model=backwards.quad, testdata = donTEST.quad) # 0.7194993
  ccr(model=backwards.quad, testdata = donTEST.quad) # 0.7550051

###################################
# ALL SECOND DEGREE TERMS ADDED (interaction and quadratic)
###################################

donTRAINING2 <- donTRAINING_orig
donTEST2 <- donTEST_orig

logModelInter <- glm(donated ~ . -targdol, data = donTRAINING2, family=binomial)
sum.mod2 <- summary(logModelInter)

num.pred2 <- nrow(sum.mod2$coefficients)

auc(model=logModelInter, testdata=donTEST2, testresponse = donTEST2$donated) # 0.7939676
ccr(model=logModelInter, testdata=donTEST2, testresponse = donTEST2$donated) # 0.7392989

#####################################################
#####################################################
######################################################
# MULTIPLE LINEAR REGRESSION W/ DONATION > 0
# http://stats.stackexchange.com/questions/28730/does-it-make-sense-to-add-a-quadratic-term-but-not-the-linear-term-to-a-model
######################################################
#####################################################
#####################################################

# now only want where TARGDOL > 0
  
  # what to do regression on
  keepcols.lm <- c("targdol", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST",  "CNDOL2", "
                CNDOL3", "CNTMLIF", "SEX", "CNMON1", "CNMONF", "CNMONL", "ContType1",
                   "SolType1", "avg", "avgTime", "don2", "don3","incr_don","Region")

  donTRAINING.lm <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.lm)]
  donTEST.lm <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.lm)]
  donTRAINING.lm <- donTRAINING.lm[donTRAINING.lm$targdol > 0,]
  donTEST.lm <- donTEST.lm[donTEST.lm$targdol > 0,]
  
  lmModel <- lm(targdol ~ . , data = donTRAINING.lm)
  sum.mod.lm <- summary(lmModel)
  
  num.pred.lm <- nrow(sum.mod.lm$coefficients)
  
  r2.lm <- sum.mod.lm$r.squared # 0.8389792

##########################
# BACKWARDS STEPWISE - LM
##########################
  
  backwards.lm = step(lmModel)
  sum.back.lm = summary(backwards.lm)
  
  r2.lm.back <- sum.back.lm$r.squared # 0.838841

  ' final w/ NA
   targdol ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + 
    CNDOL2 + CNTMLIF + CNMONF + CNMONL + avg + don2 + don3 + 
    incr_don + ContType1 + SolType1'

##########################
# ADD QUADRATIC - LM W/ QUADRATIC
##########################

  keepcols.quad <- c("targdol", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDOL2","CNDOL3", 
                     "CNTMLIF",  "SEX", "CNMON1", "CNMONF", "CNMONL", "ContType1", 
                     "SolType1", "avg", "avgTime", "don2", "don3", "incr_don", "Region", "sq_CNDOL1", 
                     "sq_CNTRLIF", "sq_CONLARG", "sq_CONTRFST", 
                        "sq_CNDOL2", "sq_CNDOL3", "sq_CNTMLIF",  
                     "sq_CNMON1",  
                     "sq_CNMONF", "sq_CNMONL", "sq_avg", "sq_avgTime")


  donTRAINING.lm.quad <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.quad)]
  donTEST.lm.quad <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.quad)]
  donTRAINING.lm.quad <- donTRAINING.lm.quad[donTRAINING.lm.quad$targdol > 0,]
  donTEST.lm.quad <- donTEST.lm.quad[donTEST.lm.quad$targdol > 0,]
  
  lmModel.quad <- lm(targdol ~ . , data = donTRAINING.lm.quad)
  sum.mod.lm.quad <- summary(lmModel.quad)
  
  num.pred.lm.quad <- nrow(sum.mod.lm.quad$coefficients)
  
  r2.lm.quad <- sum.mod.lm.quad$r.squared # 0.8804493

##########################
# BACKWARDS STEPWISE LM W/ QUADRATIC
##########################

  backwards.lm.quad = step(lmModel.quad)
  sum.back.lm.quad = summary(backwards.lm.quad)
  
  r2.lm.back.quad <- sum.back.lm.quad$r.squared # 0.8803298
  
  'final w/ NA
  targdol ~ sq_CNDOL1 + sq_CNTRLIF + sq_CONLARG + 
    sq_CONTRFST + sq_CNDOL2 + sq_CNDOL3 + sq_CNTMLIF + sq_CNMON1 + 
    sq_CNMONL + sq_avg + sq_avgTime + CNDOL1 + CNTRLIF + CONLARG + 
    CONTRFST + CNDOL2 + CNDOL3 + CNTMLIF + CNMONF + CNMONL + 
    avg + avgTime + don2 + don3 + incr_don + SEX + ContType1 + 
    SolType1
  '
###################################
# ALL SECOND DEGREE TERMS ADDED
###################################

donTRAINING.lm.inter <- donTRAINING_orig
donTEST.lm.inter <- donTEST_orig
donTRAINING.lm.inter <- donTRAINING.lm.inter[donTRAINING.lm.inter$targdol > 0,]
donTEST.lm.inter <- donTEST.lm.inter[donTEST.lm.inter$targdol > 0,]

lmModel.inter <- lm(targdol ~ . , data = donTRAINING.lm.inter)
sum.mod.lm.inter <- summary(lmModel.inter)

num.pred.lm.inter <- nrow(sum.mod.lm.inter$coefficients)

r2.lm.inter <- sum.mod.lm.inter$r.squared # 0.9517647

#####################################################
#####################################################
######################################################
# EXPECTED DONATIONS
######################################################
#####################################################
#####################################################
  
  logmodelnames = c("logModel", "logModel.quad","backwards","backwards.quad", "logModelInter")
  logmodels = list(logModel, logModel.quad, backwards, backwards.quad, logModelInter)
  lmmodelnames = c("lmModel", "lmModel.quad", "backwards.lm.quad", "backwards.lm", "lmModel.inter")
  lmmodels = list(lmModel, lmModel.quad, backwards.lm.quad, backwards.lm, lmModel.inter)
  
  don.output <- data.frame("","",0,stringsAsFactors=FALSE)
  colnames(don.output) <- c("logmodel","lmmodel","exp.don")
  for(i in 1:length(logmodelnames)){
    for(j in 1:length(lmmodelnames)){
     newcol <- c(logmodelnames[i],
                 lmmodelnames[j],
                 expected.don(final.log.model = logmodels[[i]],
                                           final.lm.model = lmmodels[[j]]))
     don.output <- rbind(don.output,newcol)
    }
  }
  don.output <- don.output[2:nrow(don.output),]
  don.output$exp.don <- as.numeric(don.output$exp.don)
  # ORDER GREATEST TO LEAST
  don.output[order(-don.output$exp.don),]
  
  '
NEW
 logmodel           lmmodel  exp.don
8   logModel.quad      lmModel.quad 10123.23
9   logModel.quad backwards.lm.quad 10102.23
26  logModelInter     lmModel.inter 10070.73
19 backwards.quad backwards.lm.quad 10047.23
18 backwards.quad      lmModel.quad 10032.23
23  logModelInter      lmModel.quad 10028.73
24  logModelInter backwards.lm.quad 10018.73
21 backwards.quad     lmModel.inter  9975.23
14      backwards backwards.lm.quad  9957.23
11  logModel.quad     lmModel.inter  9938.23
4        logModel backwards.lm.quad  9917.23
13      backwards      lmModel.quad  9912.23
3        logModel      lmModel.quad  9892.23
6        logModel     lmModel.inter  9777.23
16      backwards     lmModel.inter  9760.23
22  logModelInter           lmModel  9624.34
25  logModelInter      backwards.lm  9624.34
17 backwards.quad           lmModel  9602.23
7   logModel.quad           lmModel  9589.23
10  logModel.quad      backwards.lm  9546.23
20 backwards.quad      backwards.lm  9534.23
5        logModel      backwards.lm  9435.23
15      backwards      backwards.lm  9410.23
2        logModel           lmModel  9400.23
12      backwards           lmModel  9380.23

OLD
  logmodel           lmmodel         exp.don
  11      backwards      lmModel.quad 10002.229995725
  15 backwards.quad      lmModel.quad 10007.549995425
  8   logModel.quad backwards.lm.quad  10010.47999573
  7   logModel.quad      lmModel.quad 10022.549995425
  5        logModel      backwards.lm  9674.229995725
  2        logModel           lmModel  9714.229995725
  13      backwards      backwards.lm  9719.229995725
  14 backwards.quad           lmModel  9784.229995725
  10      backwards           lmModel  9789.229995725
  6   logModel.quad           lmModel  9844.229995725
  17 backwards.quad      backwards.lm  9877.229995725
  9   logModel.quad      backwards.lm  9892.229995725
  4        logModel backwards.lm.quad   9922.15999603
  12      backwards backwards.lm.quad   9927.15999603
  3        logModel      lmModel.quad  9967.229995725
  16 backwards.quad backwards.lm.quad   9995.47999573
  '
  # http://stackoverflow.com/questions/18177764/make-list-of-glm-in-r
  
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
  
####################################################
#####################################################
######################################################
# TO DO
######################################################
#####################################################
#####################################################

# 1. outliers/influential vars

  # 1500 is def an influential obs - keep? exclude?

# 2. normality/heteroscedasticity assuumptions

# 3. quadratic terms? can look @ each var against targdol or something.
  #### ALL QUADRATIC TERMS KEPT IN THE MODEL
  #### SEE IF QUADRATIC RELATIONSHIP EXISTS (DOES IT MAKE SENSE ?)
    # looks like it doe
  alldata <- rbind(donTEST_orig,donTRAINING_orig)
  cols <- c("TARGDOL","CNDOL1", "CONTRFST",  
   "CNDOL2", "CNDOL3", "CNMONL", "avg")
  alldata <- alldata[cols]
  alldata <- alldata[alldata$TARGDOL < 250,]
  
  plotcols <- colnames(alldata)[colnames(alldata) != "TARGDOL"]
  for(i in 1:length(plotcols)){
    plot(x=alldata[,plotcols[i]],
         y=alldata$TARGDOL,
         xlab = plotcols[i],
         ylab = c("TARGDOL"))
  }
  
  
  
  
  
###################################################
###################################################
###################################################

#dylan adding second degree terms
donTRAINING2 <- addSecondDegree(donTRAINING)
donTEST2 <- addSecondDegree(donTEST)

library(FSelector)
weights <- information.gain(donated~., donTRAINING2)
weights$names <- rownames(weights)
weights <- weights[order(-weights$attr_importance),]
top10 <- weights$names[1:139]
g <- as.simple.formula(top10, "donated")
f <- as.formula(paste("donated ~ ", paste(top10, collapse= "+")))
logModel2 <- glm(f, data = donTRAINING2, family=binomial)
logModel3 <- glm(donated~., data = donTRAINING2, family=binomial)

expected.don(final.log.model = logModel3, final.lm.model = lmModel, final.data = donTEST2)


