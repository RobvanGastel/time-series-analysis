library(fpp2)
library(gridExtra)
library(nortest)

# Generating white noise
y <- ts(rnorm(50))
ggtsdisplay(y)

# Applying naive forecasting as comparison

# Without seasonality
# Averaging method, Use mean as forecast
y.mean <- meanf(y)
y.p1 <- autoplot(y.mean)

# Naive method or Random Walk, Use the last observation of as forecast
y.naive <- naive(y)
y.p2 <- autoplot(y.naive)

# Random Walk w/ Drift, Last observation plus average change
y.dnaive <- rwf(y, drift=TRUE)
y.p3 <- autoplot(y.dnaive)

# With seasonality
# Seasonal Naive method, Use the last observation of the same season as forecast
y.snaive <- snaive(y)
y.p4 <- autoplot(y.snaive)

# Compare plots
grid.arrange(y.p1, y.p2, y.p3, y.p4, ncol = 4)

# Generate white noise with drift
z <- arima.sim(model= list(order = c(0, 1, 0)), n=200, mean=1,sd=5)
autoplot(z)

# Residuals
checkresiduals(y.naive)

# Examples of Box test
Box.test(z) # Not significant
Box.test(y) # Significant

# ACF useful for residuals
ggAcf(y.naive$residuals)




