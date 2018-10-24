library(tidyverse)
library(tidyquant)
library(quantmod)
library(rugarch)


#downloading the relevant data from yahoo
yahoo <- function(ticker, start, end) {
  tq_get(ticker, get = "stock.prices", from = start, to = end) %>%
    select(date, adjusted) %>% 
    mutate( LogPrice = log(adjusted), LogReturn = c(0,diff(LogPrice)))
}  

#Plain GARCH model, underlying is arma(p,q)
underlying_arma <- function(p,q){  
  ugarchspec (
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(p, q)),
      distribution.model = "norm"
    )
}
SimEndReturns <- function(ticker, start = "2014-10-01", end = "2018-10-01", p = 1, q = 1, nsim = 63, msim = 1000){
  #fitting garch to the stock return data
  stock_data <- yahoo(ticker, start, end)
  modelfit <-  ugarchfit(data=data.frame(stock_data)[, 4], spec = underlying_arma( p, q), solver = "solnp", 
                       fit.control=list(scale=1),out.sample=2 , solver.control=list(trace=1)) 
  #simulating 1000 forecast horizons for one quarter, 63 days
  modelsim <- ugarchsim(fit = modelfit, n.sim = nsim, n.start = 0, m.sim = msim, 
                       startMethod = "sample")
  #the cumulated return for the end of the 63. day, simulated 1000 times
  endprice_vector <- exp(colSums(modelsim@simulation$seriesSim))
}

#pricing the option based on E(max(S1 - K, 0))
EvOptionPricer <- function(ticker, start = "2014-10-01", end = "2018-10-01", p = 1, q = 1, days_left = 63, msim = 1000, opt){
  stock_data <- yahoo(ticker, start, end)
  current_price = tail(stock_data, n = 1)[2]
  end_returns <- SimEndReturns(ticker, start, end,  p, q, nsim = days_left, msim = msim)
  end_prices = end_returns * as.numeric(current_price)
  dif <- end_prices - as.numeric(current_price)
  if (opt == "call") dif[dif < 0] = 0
  if (opt == "put") {dif[dif > 0] = 0; dif = -1 * dif}
  return (round(mean(dif), 2))
}

#historical volatility for the BS model
hist_vol <- function(ts, days_in_y = 250){
  return(sqrt(var(ts) * days_in_y))
}

#pricing the option based on the BS model
BsOptPricer <- function(ticker, start = "2014-10-01", end = "2018-10-01", days_left = 63, r, opt){
  stock_data <- yahoo(ticker, start, end)
  rets <- stock_data[,4]
  vol <- hist_vol(rets)
  S = as.numeric(tail(stock_data, n = 1)[2])
  K = S
  r = log(as.numeric( tail( getSymbols('DGS3MO',src = 'FRED', from = "2018-10-01", to = Sys.Date(), auto.assign = F), n = 1))/100 + 1)
  d1 <- (log(S / K) + (r + vol ^ 2 / 2) * days_left / 250) /
    (vol * sqrt(days_left / 250))
  d2 <- d1 - vol * sqrt(days_left / 250)
  if (opt == "call"){
    price <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * days_left / 250)}
  else {
    price <- pnorm(-1*d2) * K * exp(-r * days_left / 250) - pnorm(-1*d1) * S}
  return(as.numeric(price))
}


tickers <- c("AMD", "AMZN", "AAPL", "BAC", "MSFT")
tibble(ticker = tickers) %>% mutate( EmpiricalCallPrice = Vectorize(EvOptionPricer)(ticker = ticker, opt = "call")) %>%
  mutate( EmpiricalPutPrice = Vectorize(EvOptionPricer)(ticker = ticker, opt = "put")) %>%
  mutate( BsCallPrice = Vectorize(BsOptPricer)(ticker = ticker, opt = "call")) %>%
  mutate( BsPutPrice = Vectorize(BsOptPricer)(ticker = ticker, opt = "put"))

