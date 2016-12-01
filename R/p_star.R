
##### write function such that for a given logistic model, we can find the 
##### optimal p* to minimize the "cost" 

# MUST LOAD DATA FIRST
# MUST RUN MODELS FIRST
# input logistic model

# p_star()
  # - calculates "cost" (false positives + false negatives) for a sequence of p* vals
  #   and given cost values (default c1 = 1, c2 = 1) associated w/ false values
  # - output's that data frame and optimal p*
  #     - p_cost = cost & p* data
  #     - p_star_val = optimal p* to minimize cost
# plot_p_cost()
  # - plots cost curve using output of p_star() function
  # - prints optimal p* value on plot

p_star <- function(log.model=logModel,
                    actual.y=donTRAINING$donated[complete.cases(donTRAINING)],
                    c1=1,c2=1,
                    pseq=seq(0.1,0.9,.01)){
  p = pseq
  cost <- c()
  for(i in 1:length(p)){
    prob <- p[i]
    real <- actual.y
    predicted <- ifelse(log.model$fitted.values > prob,1,0)
    diff <- real - predicted
    falsepos <- length(diff[diff == -1])
    falseneg <- length(diff[diff == 1])
    Ci = c1*falsepos + c2*falseneg
    cost <- c(cost,Ci)
  }
  
  p_cost = data.frame(pseq,cost)
  p_star_val <- p_cost$pseq[which(cost == min(p_cost$cost))]
  
  returndata <- list("p_cost" = p_cost,
                     "p_star_val" = p_star_val)
  
  }


plot_p_cost <- function(p_cost_data = p_star()){
  
  plotdata <- (p_cost_data$p_cost)
  opt_p <- p_cost_data$p_star_val
  plotdata$cost <- plotdata$cost/10^3
  
  plot(x=plotdata$pseq,
       y=plotdata$cost,
       type="l",
       lty = 1,
       col = "red",
       ylim = c(min(plotdata[,-1]),max(plotdata[,-1])),
       ylab="Cost (thousands)",
       xlab = "p*",
       axes=FALSE,
       main="Cost curve and optimal p*")
  axis(side = 2, las=1)
  axis(side = 1)
  box()
  #mtext(text = "Thousands", side = 3, adj = 0, cex= 0.8)
  mtext(text = paste("optimal p* = ",opt_p,sep=""),
        side = 3, line = -2)
  
}

