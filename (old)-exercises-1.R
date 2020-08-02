library(fpp2)
#library(forecastML), package based on forecasting

# exercise 1.1
# a)
# Perform EDA
load("./data/volcano.RData")
load("./data/souvenir.RData")
# list.files("./data") #, Show list of files in given dir
dust.ts <- ts(volcanodust)
souvenir.ts <- ts(souvenir$souvenir,start=c(1987,1), frequency=12)

autoplot(dust.ts)

# Main Properties:
#   – Trend? No
#   – Seasonal Variation? No
#   – Cyclic Variation? Yes, some cycles but hard to determine how long as they're irregular
#   – Irregular Variation? Yes
#     • Sudden Changes? Yes, e.g. at the end of the series we observe barely any peaks
#     • Outliers? Yes, a large spike of above 700
#     • Missing Values? No

autoplot(souvenir.ts)

# Main Properties:
#   – Trend? Yes, growing overtime
#   – Seasonal Variation? Yes, huge spikes at the end of the year
#   – Cyclic Variation? No, relavily the same
#   – Irregular Variation? No
#     • Sudden Changes? No
#     • Outliers? No, there are some irregularities but no extreme oberservations
#     • Missing Values? No


# b) As the volcano dataset doesn't have a clear trend finite differencing might not be nessecary,
# Finite differencing, differencing = 1, 2, ...

# The suggested finite differencing order,
ndiffs(dust.ts)
# This doesn't make sense for this dust volcano data

# The souvenir dataset does display a clear trend,
# The suggested finite differencing order,
ndiffs(souvenir.ts)

# Finite differencing, differencing = 1
souvenir.diff1 <- diff(souvenir.ts, differences = 1)
autoplot(souvenir.diff1)

# We don't observe a difference between 1 and 2, differencing = 2
souvenir.diff2 <- diff(souvenir.ts, differences = 2)
autoplot(souvenir.diff2)

# Explain how finite differencing might correct for trend.
# It looks at the growth of the trend using discrete differentiation?

# c) For the souvenir dataset, determine the period of seasonality and argue if an
# additive or a multiplicative seasonal model seems most adequate.
ggseasonplot(souvenir.ts, season.labels=TRUE, year.labels=TRUE, col=rainbow(12))
ggmonthplot(souvenir.ts)

# As we've seen this should be modeld as a multiplicative model, 
souvenir.deco <- decompose(souvenir.ts, type="multiplicative")
autoplot(souvenir.deco)

# This doesn't look completely random, with 2 outliers
autoplot(souvenir.deco$random)

souvenir.decodiff <- decompose(souvenir.diff1, type="multiplicative")
autoplot(souvenir.decodiff)

ggtsdisplay(souvenir.decodiff$random)
# There is 1 outlier,
autoplot(souvenir.decodiff$random)

# d) Inspect seasonal differencing
nsdiffs(souvenir.ts) # Suggested seasonal order number

souvenir.sdiff <- diff(souvenir.ts, differencing = 1, lag = 12)

autoplot(souvenir.sdiff)
ggtsdisplay(souvenir.sdiff)

# e) Combine both finite and seasonal differencing
souvenir.diff <- diff(souvenir.ts, differencing = 1)
autoplot(souvenir.diff)

souvenir.sdiff <- diff(souvenir.diff, differencing = 1, lag = 12)
autoplot(souvenir.sdiff)

# f) 
# Both direct and indirect correlation
ggtsdisplay(souvenir.sdiff)

souvenir.deco <- decompose(souvenir.sdiff, type="multiplicative")
autoplot(souvenir.deco)

# We see a pattern in the residuals with one huge outlier
autoplot(souvenir.deco$random)

# g)
ggtsdisplay(souvenir.sdiff)

souvenir.deco <- decompose(souvenir.sdiff, type="additive")
autoplot(souvenir.deco)

# Observe more outliers
autoplot(souvenir.deco$random)

# exercise 1.2
# a)
ggtsdisplay(souvenir.ts)

# Seperate functions using 12 months, as the time lag
pacf(souvenir.ts, lag.max = 12)
acf(souvenir.ts, lag.max = 12)

mean(acf(souvenir.ts, lag.max = 12)$acf)
mean(pacf(souvenir.ts, lag.max = 12)$acf)

# b)
autoplot(souvenir.ts)
# Looks like a small trend in the data and a strong
# seasonality pattern

# Suggested to finite differentiate by 1 for trend,
ndiffs(souvenir.ts)

# Suggested is differentiating by 1 for seasonality,
nsdiffs(souvenir.ts)

souvenir.diff <- diff(souvenir.ts, differences = 1)
souvenir.sdiff <- diff(souvenir.diff, lag = 12, differences = 1)

pacf(souvenir.sdiff, lag.max = 12)
acf(souvenir.sdiff, lag.max = 12)

# c)
souvenir.ma3 <- filter(souvenir.ts,filter=rep(1/3,3),sides=2)
souvenir.ma9 <- filter(souvenir.ts,filter=rep(1/9,9),sides=2)
souvenir.spec <- spectrum(souvenir.ts)

autoplot(souvenir.ma3)
autoplot(souvenir.ma9)

# exercise 1.3
# a)

# b)
# c)


# Plotting time lag
gglagplot(dust.ts, lags=12, do.lines=F)

ggtsdisplay(dust.ts)
