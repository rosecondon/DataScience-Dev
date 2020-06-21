
# Using July through October daily-high-temperature data for Atlanta for 1996 through 2015, 
# use a CUSUM approach to identify when unofficial summer ends (i.e., when the weather starts cooling off) each year. 
# You can get the data that you need from the file temps.txt or online, for example at http://www.iweathernet.com/atlanta-weather-records
rm(list = ls())
library(readr)

wdata <- read.table("temps.txt", stringsAsFactors = FALSE, header = T) # has V1 - VN, and Year
#wdata <- read.delim("temps.txt", header=T)
head(wdata)
nrow(wdata)

X <-  rnorm(wdata) # Observation number
# Compute the estimates needed for running the chart - in this case μ̂  and σ - mean and standard diviation
library(spcadjust)
chart <- new("SPCCUSUM",model=SPCModelNormal(Delta=1));
xihat <- xiofdata(chart,X)
str(xihat)

# compute a threshold that with roughly 90% probability results in an average run length of at least 100 in control
cal <- SPCproperty(data=X,nrep=50,
                   property="calARL",chart=chart,params=list(target=nrow(wdata)),quiet=TRUE)
cal

# Run our CUSUM chart
S <- runchart(chart, newdata=X,xi=xihat)
S

plot(S,ylab=expression(S[t]),xlab="Times Intervals",type="l",ylim=range(S,cal@res+1,cal@raw))
lines(c(0,100),rep(cal@res,2),col="red")
lines(c(0,100),rep(cal@raw,2),col="blue")
legend("topleft",c("Adjusted Threshold","Unadjusted Threshold"),col=c("red","blue"),lty=1)

# Create a new data frame base on the original one, calculate the average temperature then add to a new column
wdata_new <- cbind(wdata)
wdata_new$Avg_Temp = rowMeans(wdata_new[,c(-1)])
head(wdata_new)

# I will also need mean for each row and a constant C for small ajustment 
wdata_mean <- mean(rowMeans(wdata_new[,c(-1)]))
# cat(wdata_mean)
avg_v <- wdata_new$Avg_Temp
C <- 4
avg_v_minus_i <- avg_v - wdata_mean - C
head(avg_v_minus_i)
cusum <- rep(0,nrow(wdata)) # initial new vect with all zeros 
for (i in 1:nrow(wdata))
{
  indicator <- cusum[i] + avg_v_minus_i[i]
  ifelse(indicator > 0, cusum[i+1] <- indicator, cusum[i+1] <- 0)
}
cat(cusum)
dates <- which(cusum >= 83) # Set 83 as threshole base on value of mean
cat(dates)

v <- character()
for (i in 1:nrow(wdata)) {
  for(j in 1:length(dates)) {
    if(i==dates[j]) {
      v[j] <- wdata_new[i, 1]
    }
  }
}
cat(v)



