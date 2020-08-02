library(fpp2)
library(gridExtra)
library(nortest)

# Generating white noise
y <- ts(rnorm(50))
ggtsdisplay(y)

# Applying naive forecasting as comparison

# Use mean as forecast
y.mean <- meanf(y)
y.p1 <- autoplot(y.mean)

# Forecast the last observed of the observation
y.naive <- naive(y)
y.p2 <- autoplot(y.naive)

# Forecast the last observed from the same season of the year
y.snaive <- snaive(y)
y.p3 <- autoplot(y.snaive)

# Forecast using the last observation plus the drift in mean
y.dnaive <- rwf(y, drift=TRUE)
y.p4 <- autoplot(y.dnaive)

# Compare plots
grid.arrange(y.p1, y.p2, y.p3, y.p4, ncol = 4)

# Generate white noise with drift
z <- arima.sim(model= list(order = c(0, 1, 0)), n=200, mean=1,sd=5)
autoplot(z)

autoplot(rwf(z, drift=TRUE))
