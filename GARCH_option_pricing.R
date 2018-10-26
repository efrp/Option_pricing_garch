library(tidyverse)
library(tidyquant)
library(quantmod)
library(rugarch)

####This is a code which fits an arma(1-1)-Garch(1,1) model to an arbitrary company's share prices' time series
####Based on the fit, I simulate trajectories, and I price a call and a put option for that company's share.
#### Comparison with BS prices and ggplots are made.


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
  c <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * days_left / 250)
  p <- pnorm(-1*d2) * K * exp(-r * days_left / 250) - pnorm(-1*d1) * S
  price <- (opt == "call") * c + (opt == "put") * p
  return(as.numeric(price))
}

tickers <- c("AMD", "AMZN", "AAPL", "BAC", "MSFT")
tibble(ticker = tickers) %>% mutate( EmpiricalCallPrice = Vectorize(EvOptionPricer)(ticker = ticker, opt = "call")) %>%
  mutate( EmpiricalPutPrice = Vectorize(EvOptionPricer)(ticker = ticker, opt = "put")) %>%
  mutate( BsCallPrice = Vectorize(BsOptPricer)(ticker = ticker, opt = "call")) %>%
  mutate( BsPutPrice = Vectorize(BsOptPricer)(ticker = ticker, opt = "put"))

###################INTERESTING PLOTS#######################
EvOptionPricer_plot <- function(ticker, start = "2014-10-01", end = "2018-10-01", p = 1, q = 1, K, days_left = 63, msim = 1000, opt){
  stock_data <- yahoo(ticker, start, end)
  current_price = tail(stock_data, n = 1)[2]
  end_returns <- SimEndReturns(ticker, start, end,  p, q, nsim = days_left, msim = msim)
  end_prices = end_returns * as.numeric(current_price)
  dif <- end_prices - K
  if (opt == "call") dif[dif < 0] = 0
  if (opt == "put") {dif[dif > 0] = 0; dif = -1 * dif}
  return (round(mean(dif), 2))
}
BsOptPricer_plot <- function(ticker, start = "2014-10-01", end = "2018-10-01", K, days_left = 63, r, opt){
  stock_data <- yahoo(ticker, start, end)
  rets <- stock_data[,4]
  vol <- hist_vol(rets)
  S = as.numeric(tail(stock_data, n = 1)[2])
  r = log(as.numeric( tail( getSymbols('DGS3MO',src = 'FRED', from = "2018-10-01", to = Sys.Date(), auto.assign = F), n = 1))/100 + 1)
  d1 <- (log(S / K) + (r + vol ^ 2 / 2) * days_left / 250) /
    (vol * sqrt(days_left / 250))
  d2 <- d1 - vol * sqrt(days_left / 250)
  c <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * days_left / 250)
  p <- pnorm(-1*d2) * K * exp(-r * days_left / 250) - pnorm(-1*d1) * S
  price <- (opt == "call") * c + (opt == "put") * p
  return(as.numeric(price))
}

ploty <- function(ticker, opt = 0, is_emp = 10, is_hist = 0, is_diff = 0){
  if (is_emp == 1){
    data <- data.frame( K = seq(50,500,12.5)) %>% mutate( EmpCallPrice = Vectorize(EvOptionPricer_plot)(ticker = ticker, K = K, opt = opt))
    p <- ggplot(data, aes(x = K, y = EmpCallPrice)) + geom_point() + geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = F ) +
      labs(title= paste(ticker, "estimated", opt, "option price versus strike price"), subtitle = "Arma(1,1)-Garch(1,1) and cubic spline smoothing", x = "Strike K", y = "Call Price")+
      ggsave(paste0(ticker,opt,is_emp,".png"))
    print(p)}
  else if (is_emp == 0) {
    data_bs <- data.frame( K = seq(50,500,12.5)) %>% mutate( EmpCallPrice = Vectorize(BsOptPricer_plot)(ticker = ticker, K = K, opt = opt))
    p <- ggplot(data_bs, aes(x = K, y = EmpCallPrice)) + geom_point() + geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = F ) +
      labs(title= paste(ticker, "theoretical", opt, "option price versus strike price"), subtitle = "Black-Scholes model and cubic spline smoothing", x = "Strike K", y = "Call Price")+
      ggsave(paste0(ticker, opt, is_emp,".png"))
    print(p)  
  }
  if (is_hist ==1){
    p <- qplot(SimEndReturns(ticker),
          geom = "histogram", 
          binwidth = .0125, 
          main = paste("Histogram for the simulated", ticker, "returns after a quarter year"), 
          xlab = "Return", fill = I("orange"), col = I("red"), xlim = c(0.7, 1.7))
    print(p)
  }
  if (is_diff == 1){
    d1 <- data.frame( K = seq(50,500,12.5)) %>% mutate( EmpCallPrice = Vectorize(EvOptionPricer_plot)(ticker = ticker, K = K, opt = opt))
    d2 <- data.frame( K = seq(50,500,12.5)) %>% mutate( BSCallPrice = Vectorize(BsOptPricer_plot)(ticker = ticker, K = K, opt = opt))
    d = data.frame( K = seq(50,500,12.5), Difference = d1$EmpCallPrice - d2$BSCallPrice)
    p <- ggplot(d, aes(x = K, y = Difference)) + geom_point() + geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = F ) +
      labs(title= paste(ticker, "estimated", opt, "option price subtracted by the theoretical one"), subtitle = "ARMA(1,1)-Garch(1,1) - Black-Scholes model and cubic spline smoothing", x = "Strike K", y = "Difference in price")+
      ggsave(paste0(ticker, opt, is_emp, is_diff, ".png"))
    print(p)
  }
}

ploty("AAPL", is_hist = 1)
ploty("AAPL", "call", 1)
ploty("AAPL", "call", 0)
ploty("AAPL", opt = "call", is_diff = 1)
