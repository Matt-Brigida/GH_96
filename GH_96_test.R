## The Gregory and Hansen (1996) test for cointegration in the presence of a single regime shift of unknown timing in the slope and/or the intercept.  

library(EIAdata)
library(tseries) 

## Get EIA API key ----
key <- source('~/eia_key')$value

## getting data to test ----
ng <- getEIA('NG.RNGWHHD.W', key=key)
oil <- getEIA('PET.RCLC1.W', key = key)

## merge so we have data over a common time interval ----
data <- merge.xts(ng, oil, join = 'inner')
names(data) <- c("ng", "oil")

oil <- log(data$oil)
gas <- log(data$ng)

## create a lower triangular matrix which has 1 on all the nonzero entries.  There is certainly a non-'for loop' way to do this. ----
ind <- matrix(0, nrow=length(oil), ncol=length(oil))
for(i in 1:length(oil)){
    ind[,i] <- c(rep(0,i),rep(1,(length(oil)-i)))}

## run an Augmented Dickey-Fuller test on the residuals of every regression ----
## each regression has a structural break at a different time over all possible times ----
adf.m2 <- 0
for(i in 1:length(oil)){
    adf.m2[i] <- adf.test(lm(as.vector(gas)~ind[,i]+as.vector(oil))$resid)$statistic}

testStat <- min(adf.m2)

## Now need to compare stat to the critical value ----


