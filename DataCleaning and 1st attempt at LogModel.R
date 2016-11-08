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
donData$avgTime <- with(donData, ifelse(is.finite((CNMONF - CNMON1)/ (CNTMLIF-1)),(CNMONF - CNMON1)/ (CNTMLIF-1) ,NA)   )
donData$avgTime[donData$avgTime==0] <- NA


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
donTRAINING <- subset( donData[-TESTindices,], select=-c(STATCODE,TARGDOL,ID,CNDAT1,CNDAT2,CNDAT3,CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )
donTEST <- subset( donData[TESTindices,] , select=-c(STATCODE, TARGDOL,ID,CNDAT1,CNDAT2,CNDAT3, CNCOD1,CNCOD2,CNCOD3, SLCOD1,SLCOD2,SLCOD3) )


logModel <- glm(donated ~ . , data = donTRAINING)
summary(logModel)

#install.packages('ROCR')
library(ROCR)
# make predictions on TEST set then give AUC
p <- predict(logModel2, newdata=donTEST, type="response")
pr <- prediction(p, donTEST$donated)
prf <- performance(pr, measure = "auc")
prf@y.values[[1]]

# confusion matrix
table(donTEST$donated,p>0.5)



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

