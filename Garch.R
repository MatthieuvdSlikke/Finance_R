##########################################################
# DATACAMP: Garch Models
##########################################################
library(xts)
library(PerformanceAnalytics)

# Plot daily S&P 500 prices
plot(sp500prices)

# Compute daily returns
sp500ret <- CalculateReturns(sp500prices)

# Check the class of sp500ret
class(sp500ret)

# Plot daily returns
plot(sp500ret)

# Compute the daily standard deviation for the complete sample   
sd(sp500ret)

# Compute the annualized volatility for the complete sample
sd(sp500ret) * sqrt(252)

# Compute the annualized standard deviation for the year 2009 
sqrt(252) *sd(sp500ret["2009"])

# Compute the annualized standard deviation for the year 2017 
sqrt(252) *sd(sp500ret["2017"])

# Showing two plots on the same figure
par(mfrow=c(2,1)) 

# Compute the rolling 1 month estimate of annualized volatility

#Set the width argument such that you compute the rolling estimate of 
#annualized volatility using windows of 22 days and 66 days.

chart.RollingPerformance(R = sp500ret["2000::2017"], width = 22,
                         FUN = "sd.annualized", scale = 252, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000::2017"], width = 66,
                         FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")

# Compute the mean daily return
m <- mean(sp500ret)

# Define the series of prediction errors
e <- sp500ret - m

# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(abs(e))

# Plot the acf of the absolute prediction errors
acf(abs(e))

# Compute the predicted variances
predvar[1] <- var(sp500ret) 
for(t in 2:nobs){
  predvar[t] <- omega + alpha * e2[t-1] + beta * predvar[t-1]
}

# Create annualized predicted volatility
ann_predvol <- xts(sqrt(252) * sqrt(predvar), order.by = time(sp500ret))

# Plot the annual predicted volatility in 2008 and 2009
plot(ann_predvol["2008::2009"], main = "Ann. S&P 500 vol in 2008-2009")