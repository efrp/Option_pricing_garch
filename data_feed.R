library(tidyverse)
library(magrittr)
library(quantmod)
library(plyr)
library(dplyr)


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
data[,c("Date", "AMD", "AMZN", "AAPL", "BAC", "EBAY")]
