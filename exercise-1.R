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
#   – Irregular Variation? Yes
#     • Sudden Changes? No
#     • Outliers? No, there are some irregularities but no extreme oberservations
#     • Missing Values? No


# b) As the volcano dataset doesn't have a clear trend finite differencing might not be nessecary,
# Finite differencing, differencing = 1, 2, ...
dust.trend1 <- diff(dust.ts, differences = 1)
autoplot(dust.trend1)
# This doesn't make sense for this dust volcano data

# The suggested finite differencing order,
ndiffs(dust.ts)


# The souvenir dataset does display a clear trend,
# The suggested finite differencing order,
ndiffs(souvenir.ts)

# Finite differencing, differencing = 1, 2, ...
souvenir.trend1 <- diff(souvenir.ts, differences = 1)
autoplot(souvenir.trend)

# We don't observe a difference between 1 and 2
souvenir.trend2 <- diff(souvenir.ts, differences = 2)
autoplot(souvenir.trend)

# TODO: Explain how finite differencing might correct for trend.


# c) For the souvenir dataset, determine the period of seasonality and argue if an
# additive or a multiplicative seasonal model seems most adequate.


# d)
# e)
# f)
# g)


# exercise 1.2
# a)
# b)
# c)


# exercise 1.3
# a)
# b)
# c)


# exercise 1.4-1.7

# Plotting time lag
gglagplot(dust.ts, lags=12, do.lines=F)

ggtsdisplay(dust.ts)