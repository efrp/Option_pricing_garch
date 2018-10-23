install.packages("tidyquant")
library(tidyverse)
library(tidyquant)
library(magrittr)
library(quantmod)
library(plyr)
library(dplyr)
install.packages("rugarch")
library(rugarch)

start <- as.Date("2014-10-01")
end <- as.Date("2018-10-01")

yahoo <- function(ticker, start, end) {
  tq_get(ticker, get = "stock.prices", from = start, to = end) %>%
    select(date, adjusted) %>% 
    mutate( LogPrice = log(adjusted), LogReturn = c(0,diff(LogPrice)))
}  
acf(yahoo("AAPL", start, end)[,4])
pacf(yahoo("AAPL", start, end)[,4], lag.max = 100)
aapl_data = yahoo("AAPL", start, end)

#Plain GARCH model, underlying is arma(1,1)
model=ugarchspec (
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1)),
  distribution.model = "norm"
)

SimEndReturns <- function(stock_data, nsim = 63, msim = 1000){
  #fitting garch to the stock return data
  modelfit = ugarchfit(data=data.frame(stock_data)[, 4], spec = model, solver = "solnp", 
                       fit.control=list(scale=1),out.sample=2 , solver.control=list(trace=1)) 
  #simulating 1000 forecast horizons for one quarter, 63 days
  modelsim = ugarchsim(fit = modelfit, n.sim = nsim, n.start = 0, m.sim = msim, 
                       startMethod = "sample")
  #the cumulated return, which is just a sum
  endprice_vector = exp(colSums(modelsim@simulation$seriesSim))
}
#testing out the function
end_returns <- SimEndReturns(aapl_data)

#pricing the option based on E(max(S1 - K, 0))
EvOptionPricer <- function(stock_data, K, days_left = 63, msim = 1000){
  current_price = as.numeric(stock_data[dim(stock_data)[1], 2])
  end_returns <- SimEndReturns(stock_data, nsim = days_left, msim = msim)
  end_prices = end_returns * current_price
  dif <- end_prices - K
  dif[dif < 0] = 0
  mean(dif)
}

EvOptionPricer(aapl_data, 225)
