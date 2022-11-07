rm(list = ls())

setwd("/home/antonsaukkonen/Desktop/PTA/com3")

data <- read.table("sales.txt", header=T, row.names=4)
colnames(data)
data

sales_ts <- ts(data$Sales, start = 1970, frequency = 12)

par(mfrow = c(3, 2))

plot(sales_ts, main="ORIGINAL")

plot(log(sales_ts), main="LOG-TRANSFORMED (Variance stabilized)")

plot(diff(sales_ts, lag=1, differences = 1), main="Lag-1; Diff-1 (Trend removed)")

plot(diff(sales_ts, lag=7, differences = 1), main="Lag-7; Diff-1 (Seasonal difference removed)")

plot(diff(sales_ts, lag=1, differences = 5), main="Lag-1; Diff-5 (P.diff trend removed)")

plot(diff(log(sales_ts), lag=7, differences = 5), main="LOG-TRANSFORMED, lag-7; diff-5")

