
## CLEANING DONATION DATA
## EXPORT TEST AND TRAINING DATA

library(dplyr)

cleandata <- function(who="kristin",
                      dropNA=TRUE){

##############################################
# READ DATA INTO R
##############################################

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
#setwd(address)

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

##############################################
#ADD NEW POTENTIALLY USEFUL COLUMNS TO DATA
##############################################

# Get region of each state
  donData <- donData %>% left_join(stateLookup, by = c('STATCODE'='abbreviation'))

# get average of all donations
  donData$avg <- donData$CNTRLIF / donData$CNTMLIF

# get average time between donations if multiple exist, otherwise NA
  donData$avgTime <- with(donData, ifelse(is.finite((CNMONF - CNMON1)/ (CNTMLIF-1)),
                                          (CNMONF - CNMON1)/ (CNTMLIF-1) ,0)   )

# add dummy variables for 2nd and 3rd contribution - potential for interaction
  donData$don2 <- with(donData, ifelse(CNDOL2 > 0,1,0))
  donData$don3 <- with(donData, ifelse(CNDOL3 > 0,1,0))

# add dummy if donation value is increasing
  # only applies if you've donated > 1x
  donData$incr_don <- ifelse(donData$CNDOL2 != 0 & (donData$CNDOL1 - donData$CNDOL2 > 0),1,0)
  
##############################################
# ADD RESPONSE VARIABLE
##############################################
# response variable for Logisitic Model
  donData$donated <- ifelse(donData$TARGDOL>0,1,0)

##############################################
# ADD QUADRATIC AND INTERACTION TERMS HERE (easier later?)
##############################################
  
  #################
  # quadratic terms
  #################
  quad.vars <- c("CNDOL1","CNTRLIF","CONLARG","CONTRFST","CNCOD1","CNCOD2","CNCOD3","CNDAT1","CNDAT2",
                 "CNDAT3","CNDOL2","CNDOL3","CNTMLIF","SLCOD1","SLCOD2","SLCOD3","CNMON1","CNMON2","CNMON3",
                 "CNMONF","CNMONL","avg","avgTime","don2","don3")
  oldcolnames <- colnames(donData)
  newcols <- paste("sq",quad.vars,sep="_")
  for(i in quad.vars){
    donData[,(ncol(donData)+1)] <- donData[,i]^2
  }
  colnames(donData) <- c(oldcolnames,newcols)
                  
  ###################
  # interaction terms
  ###################
  inter.vars <- c("CNDOL1","CNTRLIF","CONLARG","CONTRFST","CNCOD1","CNCOD2","CNCOD3","CNDAT1","CNDAT2",
                 "CNDAT3","CNDOL2","CNDOL3","CNTMLIF","SLCOD1","SLCOD2","SLCOD3","CNMON1","CNMON2","CNMON3",
                 "CNMONF","CNMONL","avg","avgTime","don2","don3")
  # possible combos
  posscombo <- combn(inter.vars,2)
  numcombo <- dim(posscombo)[2]
  # EEK THERE ARE 300...
  oldcolnames <- colnames(donData)
  # create new column names
  newcols <- c()
  for(i in 1:numcombo){
    a <- paste(posscombo[,i][1],posscombo[,i][2],sep="_")
    newcols <- c(newcols,a)
    donData[,(ncol(donData)+1)] <- donData[,posscombo[,i][1]]*donData[,posscombo[,i][2]]
  }
  colnames(donData) <- c(oldcolnames,newcols)
  
##############################################
# RETURN TEST AND TRAINING DATA
##############################################
  
  TESTindices <- seq(from = 3,to = nrow(donData), by = 3)
  
  donTRAINING <- donData[-TESTindices,]
  donTEST <- donData[TESTindices,]
  
  ## drop NAs now? - check w group
  ## ONLY COMPLETE OBS WILL BE USED IN DATA ANYWAY...
  # must do it after the split so we get the same obs
  if(dropNA){
    donTRAINING <- donTRAINING[complete.cases(donTRAINING),]
    donTEST <- donTEST[complete.cases(donTEST),]
  }
    
  
  returndata <- list("train" = donTRAINING,
                     "test" = donTEST)
  
  return(returndata)
}

#function that adds all second degree terms (besides for factor columns)
addSecondDegree <- function(myData){
  #separate response variable
  donated <- myData$donated
  myData2 <- myData[,-which(names(myData)=="donated")]
  
  #separate factors since they won't have squared terms
  facs <- sapply(myData2, is.factor)
  factorCols <- data.frame(myData2[,facs])
  myData3 <- data.frame(myData2[,!facs])
  colnames(myData3) <- colnames(myData2)[!facs]
  
  #add squared and interaction terms
  myData4 <- data.frame(model.matrix(donated~I(myData3^2) + .^2 ,data=myData3))
  
  #add back response variable
  myData4 <- cbind(myData4, donated)
  myData4$X.Intercept. <- NULL
  
  #add back factor columns
  myData4 <- cbind(myData4, factorCols)
  
  #fix column names of squared terms
  if (ncol(myData3) > 0) {
    colnames(myData4)[1:ncol(myData3)] <- paste(colnames(myData3),'^2',sep='')
  }
  return(myData4)
}

