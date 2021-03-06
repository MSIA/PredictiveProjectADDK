
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

#####################################################
#####################################################
######################################################
# LOGISTIC REGRESSION MODEL
######################################################
#####################################################
#####################################################

# fit basic logistic regression model
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Split into TRAINING and TEST sets
# Remove some columns
# "CNDOL1"    "CNTRLIF"   "CONLARG"   "CONTRFST"  "CNCOD1"    "CNCOD2"    "CNCOD3"    "CNDAT1"   
# "CNDAT2"    "CNDAT3"    "CNDOL2"    "CNDOL3"    "CNTMLIF"   "SLCOD1"    "SLCOD2"    "SLCOD3"   
# "TARGDOL"   "STATCODE"  "SEX"       "CNMON1"    "CNMON2"    "CNMON3"    "CNMONF"    "CNMONL"   
# "ID"        "ContType1" "ContType2" "ContType3" "SolType1"  "SolType2"  "SolType3"  "Region"   
# "avg"       "avgTime"   "don2"      "don3"      "donated" "incr_don"

# QUAD TERMS
# "sq_CNDOL1"   "sq_CNTRLIF"  "sq_CONLARG"  "sq_CONTRFST" "sq_CNCOD1"   "sq_CNCOD2"   "sq_CNCOD3"   "sq_CNDAT1"  
# "sq_CNDAT2"   "sq_CNDAT3"   "sq_CNDOL2"   "sq_CNDOL3"   "sq_CNTMLIF"  "sq_SLCOD1"   "sq_SLCOD2"   "sq_SLCOD3"  
# "sq_CNMON1"   "sq_CNMON2"   "sq_CNMON3"   "sq_CNMONF"   "sq_CNMONL"   "sq_avg"      "sq_avgTime"  "sq_don2"    
# "sq_don3"   

  # what to do regression on
  keepcols <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", "
                  CNDOL3", "CNTMLIF", "SLCOD1", "SLCOD2", "SEX", "CNMON1", "CNMON2", "
                  CNMON3", "CNMONF", "CNMONL", "ContType1", "SolType1", "SolType2", "
                  SolType3", "avg", "avgTime", "don2", "don3","incr_don")
  
  ## you can either have: DATES OF CONTRIBUTION or MONTHS SINCE LATEST CONTRIBUTION
  ## since all data is using 1 date as current date. these are redundant 
  
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
  
  auc(model=logModel) # 0.7766506
  ccr(model=logModel) # 0.7312671

##########################
# BACKWARDS STEPWISE
##########################

  logModel <- glm(donated ~ . , data = donTRAINING, family=binomial)
  
  backwards = step(logModel)
  summary(backwards)
  
  auc(model=backwards) # 0.7766322
  ccr(model=backwards) # 0.7306057
  
  # final
  'donated ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + 
    CNDAT1 + CNDOL2 + CNTMLIF + SLCOD1 + SEX + CNMON2 + CNMONF + 
    CNMONL + ContType1 + SolType2 + avg + incr_don'

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
  donated ~ CNMON2 + CNMONL + CNDAT1 + avgTime + 
      ContType1 + CONTRFST + incr_don + SolType2 + CNTMLIF + CNMONF + 
  CNDOL2 + CONLARG + CNDOL1 + CNTRLIF + SLCOD1 + avg + SEX
  '
  
  auc(model=forwards) # 0.776684
  ccr(model=forwards) # 0.7302277


######################################################
# QUADRATIC TERMS ADDED
# http://stats.stackexchange.com/questions/28730/does-it-make-sense-to-add-a-quadratic-term-but-not-the-linear-term-to-a-model
######################################################


# USE OUTPUT FROM STEPWISE REGRESSION + SQUARE TERMS.
  
  keepcols.quad <- c("donated", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", 
              "CNTMLIF", "SLCOD2", "SEX", "CNMON2", "CNMON3", "CNMONF", "CNMONL", "ContType1", 
              "SolType1", "SolType3", "avg", "avgTime", "incr_don", "sq_CNDOL1", 
              "sq_CNTRLIF", "sq_CONLARG", "sq_CONTRFST", "sq_CNCOD1", "sq_CNCOD2", "sq_CNCOD3", 
              "sq_CNDAT1", "sq_CNDAT2", "sq_CNDAT3", "sq_CNDOL2", "sq_CNDOL3", "sq_CNTMLIF",  
              "sq_SLCOD1", "sq_SLCOD2", "sq_SLCOD3", "sq_CNMON1", "sq_CNMON2", "sq_CNMON3", 
              "sq_CNMONF", "sq_CNMONL", "sq_avg", "sq_avgTime", "sq_don2","sq_don3")

  donTRAINING.quad <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.quad)]
  donTEST.quad <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.quad)]
  
  logModel.quad <- glm(donated ~ . , data = donTRAINING.quad, family=binomial)
  sum.mod.quad <- summary(logModel.quad)
  
  num.pred.quad <- nrow(sum.mod.quad$coefficients)
  
  auc(model=logModel.quad, testdata = donTEST.quad) # 0.7895988
  ccr(model=logModel.quad, testdata = donTEST.quad) # 0.7420391

###################################
# BACKWARDS STEPWISE - W/ QUADRATIC
###################################

  logModel.quad <- glm(donated ~ . , data = donTRAINING.quad, family=binomial)
  
  backwards.quad = backwards
  
  backwards.quad = step(logModel.quad)
  summary(backwards.quad)
    
  'final
  glm(formula = donated ~ CNDOL1 + CONLARG + CONTRFST + CNDOL2 + 
      CNTMLIF + SEX + CNMON2 + CNMON3 + CNMONF + CNMONL + ContType1 + 
  SolType3 + avg + sq_CNDOL1 + sq_CNTRLIF + sq_CNCOD1 + sq_CNCOD2 + 
  sq_CNCOD3 + sq_CNDAT1 + sq_CNDAT2 + sq_CNDAT3 + sq_CNDOL2 + 
  sq_CNTMLIF + sq_SLCOD3 + sq_CNMONF + sq_CNMONL + sq_avg, 
  family = binomial, data = donTRAINING)
  '
  
  auc(model=backwards.quad, testdata = donTEST.quad) # 0.788409
  ccr(model=backwards.quad, testdata = donTEST.quad) # 0.7382595



#####################################################
#####################################################
######################################################
# MULTIPLE LINEAR REGRESSION W/ DONATION > 0
# http://stats.stackexchange.com/questions/28730/does-it-make-sense-to-add-a-quadratic-term-but-not-the-linear-term-to-a-model
######################################################
#####################################################
#####################################################

# now only want where TARGDOL > 0

# "CNDOL1"    "CNTRLIF"   "CONLARG"   "CONTRFST"  "CNCOD1"    "CNCOD2"    "CNCOD3"    "CNDAT1"   
# "CNDAT2"    "CNDAT3"    "CNDOL2"    "CNDOL3"    "CNTMLIF"   "SLCOD1"    "SLCOD2"    "SLCOD3"   
# "TARGDOL"   "STATCODE"  "SEX"       "CNMON1"    "CNMON2"    "CNMON3"    "CNMONF"    "CNMONL"   
# "ID"        "ContType1" "ContType2" "ContType3" "SolType1"  "SolType2"  "SolType3"  "Region"   
# "avg"       "avgTime"   "don2"      "don3"      "donated" "incr_don"
  
  # what to do regression on
  keepcols.lm <- c("TARGDOL", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", "
                CNDOL3", "CNTMLIF", "SLCOD1", "SLCOD2", "SEX", "CNMON1", "CNMON2", "
                CNMON3", "CNMONF", "CNMONL", "ContType1", "SolType1", "SolType2", "
                SolType3", "avg", "avgTime", "don2", "don3","incr_don","Region")
  
  ## you can either have: DATES OF CONTRIBUTION or MONTHS SINCE LATEST CONTRIBUTION
  ## since all data is using 1 date as current date. these are redundant 

  donTRAINING.lm <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.lm)]
  donTEST.lm <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.lm)]
  donTRAINING.lm <- donTRAINING.lm[donTRAINING.lm$TARGDOL > 0,]
  donTEST.lm <- donTEST.lm[donTEST.lm$TARGDOL > 0,]
  
  lmModel <- lm(TARGDOL ~ . , data = donTRAINING.lm)
  sum.mod.lm <- summary(lmModel)
  
  num.pred.lm <- nrow(sum.mod.lm$coefficients)
  
  r2.lm <- sum.mod.lm$r.squared # 0.8935154

##########################
# BACKWARDS STEPWISE - LM
##########################
  
  backwards.lm = step(lmModel)
  sum.back.lm = summary(backwards.lm)
  
  r2.lm.back <- sum.back.lm$r.squared # 0.8933424
  
  'final
  TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CONTRFST + 
      CNDOL2 + CNTMLIF + SLCOD1 + CNMON2 + CNMONF + CNMONL + ContType1 + 
      SolType1 + SolType2 + avg + incr_don
  '

##########################
# ADD QUADRATIC - LM W/ QUADRATIC
##########################

  keepcols.quad <- c("TARGDOL", "CNDOL1", "CNTRLIF", "CONLARG", "CONTRFST", "CNDAT1", "CNDOL2", 
                     "CNTMLIF", "SLCOD2", "SEX", "CNMON2", "CNMON3", "CNMONF", "CNMONL", "ContType1", 
                     "SolType1", "SolType3", "avg", "avgTime", "incr_don", "Region", "sq_CNDOL1", 
                     "sq_CNTRLIF", "sq_CONLARG", "sq_CONTRFST", "sq_CNCOD1", "sq_CNCOD2", "sq_CNCOD3", 
                     "sq_CNDAT1", "sq_CNDAT2", "sq_CNDAT3", "sq_CNDOL2", "sq_CNDOL3", "sq_CNTMLIF",  
                     "sq_SLCOD1", "sq_SLCOD2", "sq_SLCOD3", "sq_CNMON1", "sq_CNMON2", "sq_CNMON3", 
                     "sq_CNMONF", "sq_CNMONL", "sq_avg", "sq_avgTime", "sq_don2","sq_don3")

  donTRAINING.lm.quad <- donTRAINING_orig[,(names(donTRAINING_orig) %in% keepcols.quad)]
  donTEST.lm.quad <- donTEST_orig[,(names(donTEST_orig) %in% keepcols.quad)]
  donTRAINING.lm.quad <- donTRAINING.lm.quad[donTRAINING.lm.quad$TARGDOL > 0,]
  donTEST.lm.quad <- donTEST.lm.quad[donTEST.lm.quad$TARGDOL > 0,]
  
  lmModel.quad <- lm(TARGDOL ~ . , data = donTRAINING.lm.quad)
  sum.mod.lm.quad <- summary(lmModel.quad)
  
  num.pred.lm.quad <- nrow(sum.mod.lm.quad$coefficients)
  
  r2.lm.quad <- sum.mod.lm.quad$r.squared # 0.9428208

##########################
# BACKWARDS STEPWISE LM W/ QUADRATIC
##########################

  backwards.lm.quad = step(lmModel.quad)
  sum.back.lm.quad = summary(backwards.lm.quad)
  
  r2.lm.back.quad <- sum.back.lm.quad$r.squared # 0.942701
  
  'final
  TARGDOL ~ CNDOL1 + CNTRLIF + CONLARG + CNDAT1 + 
      CNDOL2 + CNMON2 + CNMON3 + CNMONL + ContType1 + SolType1 + 
  SolType3 + avg + incr_don + sq_CNDOL1 + sq_CONTRFST + sq_CNCOD1 + 
  sq_CNCOD2 + sq_CNDAT2 + sq_CNDAT3 + sq_CNDOL2 + sq_CNDOL3 + 
  sq_SLCOD3 + sq_CNMONL + sq_avg
  '


#####################################################
#####################################################
######################################################
# EXPECTED DONATIONS
######################################################
#####################################################
#####################################################
  
  final.log.model = backwards.quad
  final.lm.model = backwards.lm.quad
  final.data = donTEST_orig
  
  #get probability that each person donates from logistic model
  final.data$prob <- predict.glm(final.log.model, newdata=final.data, type="response")
  
  #get guess of each person's donation from multiple regression model
  final.data$donGuess <- predict.lm(final.lm.model, newdata=final.data, na.action = na.pass)
  final.data$donGuess <- ifelse(final.data$donGuess < 0, 0, final.data$donGuess)
  
  #get expected value of each person's donation
  final.data$expDon <- final.data$prob * final.data$donGuess
  summary(final.data$expDon)
  
  #Select 1000 donors from the test set who have the highest E(TARGDOL). These may be the donors
  #that will be special marketing targets. Then find their total actual donations. This is the payoff
  #and should be as high as possible.
  top1000 <- final.data[order(final.data$expDon, decreasing=T),][1:1000,]
  tot.don <- sum(top1000$TARGDOL) # 9970.55


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
  cols <- c("TARGDOL","CNDOL1", "CONTRFST", "CNCOD1", "CNCOD2", "CNDAT2", 
  "CNDAT3", "CNDOL2", "CNDOL3", "SLCOD3", "CNMONL", "avg")
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

