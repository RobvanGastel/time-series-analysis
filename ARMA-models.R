library(fpp2)


# Simulation
# Create simulations with the forecasting package
# Generate SARIMA model with fixed parameters
model <- Arima(ts(rnorm(100),freq=4), order=c(1,1,1), seasonal=c(1,1,1),
               fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))

# Simulate
foo <- simulate(model, nsim=1000)

# Fit the model
fit <- Arima(foo, order=c(1,1,1), seasonal=c(1,1,1))


# Modelling
# Explore the souvenir dataset
load('./data/souvenir.RData')
ndiffs(svr.ts)
nsdiffs(svr.ts)

svr.d1 <- diff(svr.ts, differences = 1)
svr.sd1 <- diff(svr.d1, differences = 1, lag = 12)
ggtsdisplay(svr.d1)
ggtsdisplay(svr.sd1)

svr.ts <- ts(souvenir, start=c(1987,1), frequency=12)
ggtsdisplay(svr.ts)
# We have to account for the seasonality and trend in the
# data however!
# Thus a SARIMA(p,1,q)x(P, 1, Q)s fit is appropriate here!

# Looking at W_t,
# adjusted for suggested seasonality and trend
ggtsdisplay(svr.sd1)

# we see a decay for the ACF in seasonality,
# and a significance in the PACF around time-lag 12.
# Which could indicate D=0, P=1, Q=2, with s=12.
svr.sarima211 <- Arima(svr.ts, order=c(2, 1, 1), 
                       seasonal=c(0, 1, 1))
svr.sarima211
# Model parameters, are significant except
# for ar1. Which makes our model more complex, but we
# leave it in.

# In-Sample Accuracy
with(svr.sarima211, accuracy(fitted, x))

# In-Sample diagnostics
checkresiduals(svr.sarima211$residuals)
# there seems to be some structure left in the residuals
# which can be observed in the histogram and line plot.

Box.test(svr.sarima211$residuals, type="Ljung-Box")
# No structure left in ACF according to Ljung-Box test.

shapiro.test(svr.sarima211$residuals)
# Residuals are not normally distributed this has 
# effect on our CI bands.

autoplot(svr.sarima211)
# Roots seem to be inside the unit circles.

# Model forecast
svr.sarima211.fore <- forecast(svr.sarima211, h=19)
autoplot(svr.sarima211.fore)

# Automatic Model Selection
# Usually has a lower AICc score,
# but might have higher GoF measures.
svr.auto <- auto.arima(svr.ts)
# (Perform the same diagnostics for Auto)
svr.auto
svr.sarima211


# In-Sample Accuracy comparisons
with(svr.auto, accuracy(fitted, x))
with(svr.sarima211, accuracy(fitted, x))
# IC: AICc is lower for auto fit as it optimizes for
# AICc.
# Bias: Is higher for our fitted model.
# Variability: Is lower for our model compared to the
# auto fit.


# In-Sample Diagnostics comparison
checkresiduals(svr.auto$residuals)
checkresiduals(svr.sarima211$residuals)
# Relatively the same deviations.


# Forecasting comparisons
svr.auto.fore <- forecast(svr.auto, h=19)

grid.arrange(
  autoplot(svr.auto.fore),
  autoplot(svr.sarima211.fore))

