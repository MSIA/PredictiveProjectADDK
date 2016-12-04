## frequency tables
library(reshape)
library(gridExtra)
library(grid)

frequency_tables <- function(){
  
## CREATES FREQUENCY TABLES FOR A GIVEN CATEGORICAL VARIABLE
  ## ADD NEW ONES TO COMBINE AND PLOT TO .PNG 
  
# ftp://cran.r-project.org/pub/R/web/packages/gridExtra/vignettes/tableGrob.html

## frequency table for categorical variables
### SEX
### ContType1
### SolType1
### SolType2 ?

address <- '/Users/kmeier92/Documents/Northwestern/fall2016/Predictive_Analytics/PredictiveProjectADDK/'
# source R code that cleans the data as needed
source(paste(address,'R/cleandata.R',sep=""))
# functions auc & ccr
source(paste(address,'R/helper.R',sep=""))

# call function to create the 2 datasets 
thedata <- cleandata(dropNA=F)
# total data set
alldata <- rbind(thedata$train,thedata$test)

###### frequency tables
### SEX
sex.tab <- freq_table(category <- "SEX", categoryname <- "Sex")
### ContType1
cont.tab <- freq_table(category <- "ContType1", categoryname <- "Latest Contribution Code")
### SolType1
sol.tab <- freq_table(category <- "SolType1", categoryname <- "Latest Solicitation Code")

whole.tab <- rbind(sex.tab,cont.tab,sol.tab)

png("freq_tables.png")
tt <- ttheme_default(base_size = 10)
grid.table(whole.tab, theme=tt, rows=NULL)
dev.off()

####
}

############ FUNCTION

freq_table <- function(category = "ContType1",
                       categoryname = "Latest Contribution Code"){
  
  the_table <- table(alldata[,category],alldata$donated)
  df_freq <- data.frame(the_table)
  df_freq <- cast(df_freq, Var1~Var2, mean)
  df_freq$tot <- df_freq$`0` + df_freq$`1`
  df_freq <- df_freq[df_freq$tot > 0,]
  df_freq$yesprop <- round(df_freq$`1`/df_freq$tot,3)
  df_freq$Var1 <- as.character(df_freq$Var1)
  df_freq <- df_freq[order(df_freq$yesprop,decreasing = TRUE),]
  totrow <- c("Total",sum(df_freq$`0`),sum(df_freq$`1`),
              sum(df_freq$tot),
              round((sum(df_freq$`1`)/sum(df_freq$tot)),3))
  df_freq <- rbind(df_freq,totrow)
  
  df_freq <- data.frame(lapply(df_freq, as.character), stringsAsFactors=FALSE)
  df_freq$pred <- c(categoryname,rep("",nrow(df_freq)-1))
  df_freq <- df_freq[,c(6,1,2,3,4,5)]
  
  colnames(df_freq) <- c("Predictor","Category","No Donation","Donation","Total","Yes Proportion")
  
  return(df_freq)
  
}