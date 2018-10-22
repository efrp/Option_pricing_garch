library(tidyverse)
library(magrittr)
library(quantmod)
library(plyr)
library(dplyr)
library(rugarch)


start <- as.Date("2014-10-01")
end <- as.Date("2018-10-01")
ticker <- c("AMD", "AMZN", "AAPL", "BAC", "EBAY")
for (i in 1:5) {
  getSymbols(ticker[i], src = "yahoo", from = start, to = end)
}

AMD_date <- as.data.frame(AMD)
AMD_date$date <- time(AMD)
AMD <- AMD_date[,c(1,7)]
colnames(AMD) <- c("Close", "Date")

AMZN_date <- as.data.frame(AMZN)
AMZN_date$date <- time(AMZN)
AMZN <- AMZN_date[,c(1,7)]
colnames(AMZN) <- c("Close", "Date")

AAPL_date <- as.data.frame(AAPL)
AAPL_date$date <- time(AAPL)
AAPL <- AAPL_date[,c(1,7)]
colnames(AAPL) <- c("Close", "Date")

BAC_date <- as.data.frame(BAC)
BAC_date$date <- time(BAC)
BAC <- BAC_date[,c(1,7)]
colnames(BAC) <- c("Close", "Date")

EBAY_date <- as.data.frame(EBAY)
EBAY_date$date <- time(EBAY)
EBAY <- EBAY_date[,c(1,7)]
colnames(EBAY) <- c("Close", "Date")

data <- join_all(list(AMD, AMZN, AAPL, BAC, EBAY), by="Date", type='inner')
colnames(data) <- c("AMD", "Date", "AMZN", "AAPL", "BAC", "EBAY")
data<-data[,c("Date", "AMD", "AMZN", "AAPL", "BAC", "EBAY")]

for (i in 1:1006) {
data[i,-1]<-log(data[i,-1]/data[i+1,-1])
}

data<-data[1:1006,]
forecastperiod<-50
data[1007:(1006+forecastperiod),1]<-as.Date(seq(from = end+1, to = end+forecastperiod, by=1))

for (i in 2:6)
{

#plot(data[1:1006,1], data[1:1006,i], main = ticker[i-1], type="l", xlab="Date", ylab=ticker[i-1])

# Lets check for homoscedasticity (is it a WN process)
#plot(data[1:1006,1], data[1:1006,i]^2, type='l')
#Box.test(coredata(data[1:1006,i]^2), type = "Ljung-Box", lag = 12)


# Finding the best ARIMA model to the returns
data_arfima <- autoarfima(data=data[1:1006,i], ar.max=3, ma.max=3, criterion = "AIC", method = "partial", distribution.model = "norm", return.all = FALSE)
data_arfima[["fit"]]@model[["modeldata"]][["index"]]<- as.POSIXct.Date(data[1:1006,1])
best_ar<-data_arfima[["fit"]]@model[["modelinc"]][["ar"]]
best_ma<-data_arfima[["fit"]]@model[["modelinc"]][["ma"]]
include_mean_o_n<-data_arfima[["fit"]]@model[["modelinc"]][["mu"]]
print(paste0("Let's fit an ARMA(", best_ar, ",",best_ma, ") model to the ", ticker[i-1], " data."))
#plot(data[1:1006,1],data_arfima[["fit"]]@fit[["residuals"]], type='l', xlab="Date", ylab="ARMA(0,0) residuals")
#acf(data_arfima[["fit"]]@fit[["residuals"]])
#qqnorm(data_arfima[["fit"]]@fit[["residuals"]])
#plot(data[1:1006,1],data_arfima[["fit"]]@fit[["residuals"]]^2, type='l', xlab="Date", ylab="ARMA(0,0) residuals")
#acf(data_arfima[["fit"]]@fit[["residuals"]]^2)
#qqnorm(data_arfima[["fit"]]@fit[["residuals"]]^2)

# Thus the best model - based on AIC - is the ARIMA(0,0,0), mean included.
# GARCH specification and estimation

# GARCH model specification
data_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),mean.model = list(armaOrder = c(best_ar, best_ma), include.mean = include_mean_o_n), distribution.model = "norm")
# GARCH model estimation
(data_garch11_fit <- ugarchfit(spec = data_garch11_spec, data = data[1:1006,i]))
data_garch11_fit@model[["modeldata"]][["index"]]<- as.POSIXct.Date(data[1:1006,1])
#plot(data[1:1006,1],data_garch11_fit@fit[["residuals"]], type='l', xlab="Date", ylab="GARCH(1,1) residuals")
#acf(data_garch11_fit@fit[["residuals"]])
#qqnorm(data_garch11_fit@fit[["residuals"]])
#plot(data[1:1006,1],data_garch11_fit@fit[["residuals"]]^2, type='l', xlab="Date", ylab="GARCH(1,1) residuals")
#acf(data_garch11_fit@fit[["residuals"]]^2, main=paste0("ACF of GARCH(1,1) residuals^2 of ",ticker[i-1]))
#qqnorm(data_garch11_fit@fit[["residuals"]]^2)

# volatility forecasting
data_garch11_fcst <- ugarchforecast(data_garch11_fit, n.ahead = forecastperiod)

forec<-fitted(data_garch11_fcst)
data[1007:(1006+forecastperiod),i]<-fitted(data_garch11_fcst)

plot(data[,1], data[,i], main = ticker[i-1], type="l", xlab="Date", ylab=ticker[i-1])
lines(data[1007:(1006+forecastperiod),1], data[1007:(1006+forecastperiod),i], col="red")
}
