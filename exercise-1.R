# Examples of lecture 1
library(fpp2)
#library(forecastML), package based on forecasting

load("./data/kings.RData")
kings.ts <- ts(kings$kings)

autoplot(kings.ts)
gglagplot(kings.ts,lags=1,do.lines=F)