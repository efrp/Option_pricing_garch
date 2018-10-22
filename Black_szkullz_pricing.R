hist_vol <- function(ts, days_in_y = 250){
  return(sqrt(var(ts) * days_in_y))
}
hist_vol(data[,2])

BsOptPricer <- function(ts, K, days_left, r = 0){
  rets = log(ts/lag(ts))[-1]
  vol <- hist_vol(rets)
  S = ts[length(ts)]
  d1 <- (log(S / K) + (r + vol ^ 2 / 2) * days_left / 250) /
    (vol * sqrt(days_left / 250))
  d2 <- d1 - vol * sqrt(days_left / 250)
  price <- pnorm(d1) * S - pnorm(d2) * K * exp(-r * days_left / 250)
  return(price)
}

