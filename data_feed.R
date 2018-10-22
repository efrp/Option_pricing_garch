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

for (i in 2:6)
{
  plot(data[,1], data[,2], main = ticker[i-1], type="l", xlab="Date", ylab=ticker[i-1])

# Lets check for homoscedasticity (is it a WN process)
#plot(data[,1], data[,i]^2, type='l')

#Box.test(coredata(data[,i]^2), type = "Ljung-Box", lag = 12)

# Finding the best ARIMA model to the returns

data_arfima <- autoarfima(data=data[,i], ar.max=2, ma.max=2, criterion = "AIC", method = "partial", return.all = FALSE)
#plot(data[,1],data_arfima[["fit"]]@fit[["residuals"]], type='l', xlab="Date", ylab="ARMA(0,0) residuals")
#acf(data_arfima[["fit"]]@fit[["residuals"]])
#qqnorm(data_arfima[["fit"]]@fit[["residuals"]])
#plot(data[,1],data_arfima[["fit"]]@fit[["residuals"]]^2, type='l', xlab="Date", ylab="ARMA(0,0) residuals")
#acf(data_arfima[["fit"]]@fit[["residuals"]]^2)
#qqnorm(data_arfima[["fit"]]@fit[["residuals"]]^2)

# Thus the best model - based on AIC - is the ARIMA(0,0,0), mean included.
# GARCH specification and estimation

# GARCH model specification
data_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")

# GARCH model estimation
(data_garch11_fit <- ugarchfit(spec = data_garch11_spec, data = data[,i]))
#plot(data[,1],data_garch11_fit@fit[["residuals"]], type='l', xlab="Date", ylab="GARCH(1,1) residuals")
#acf(data_garch11_fit@fit[["residuals"]])
#qqnorm(data_garch11_fit@fit[["residuals"]])
#plot(data[,1],data_garch11_fit@fit[["residuals"]]^2, type='l', xlab="Date", ylab="GARCH(1,1) residuals")
#acf(data_garch11_fit@fit[["residuals"]]^2)
#qqnorm(data_garch11_fit@fit[["residuals"]]^2)

# Risk model backtesting

data_garch11_roll <- ugarchroll(data_garch11_spec, data[,i], n.start = 120, refit.every = 24, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)
## wrong dates for ugarchroll! I SIMPLY CANNOT CHANGE IT!!!
data_garch11_roll@model[["index"]]<- as.Date(data[,1])
report(data_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)


# plot backtesting graph

# First, create a zoo object using the extracted forecasted VaR from the ugarchroll object.
data_VaR <- zoo(data_garch11_roll@forecast$VaR[, 1])

# overwrite the index property of the zoo object with the rownames (year and month) from this ugarchroll object 
index(data_VaR) <- as.Date(rownames(data_garch11_roll@forecast$VaR))

# do the same for the actual returns that are also stored in the ugarchroll object.
data_actual <- zoo(data_garch11_roll@forecast$VaR[, 2])
index(data_actual) <- as.Date(rownames(data_garch11_roll@forecast$VaR))

# plot the VaR versus the actual returns of Intel 
plot(data_actual, type = "l", main = "99% 1 Month VaR Backtesting", xlab = "Date", ylab = "Return/VaR in percent")
lines(data_VaR, col = "red")
legend("topright", inset=.05, c("Returns","VaR"), col = c("black","red"), lty = c(1,1))

# volatility forecasting

data_garch11_fcst <- ugarchforecast(data_garch11_fit, n.ahead = 12)

}