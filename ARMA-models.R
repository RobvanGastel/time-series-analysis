library(fpp2)

# Generate SARIMA model with fixed parameters
model <- Arima(ts(rnorm(100),freq=4), order=c(1,1,1), seasonal=c(1,1,1),
               fixed=c(phi=0.5, theta=-0.4, Phi=0.3, Theta=-0.2))

# Simulate
foo <- simulate(model, nsim=1000)

# Fit the model
fit <- Arima(foo, order=c(1,1,1), seasonal=c(1,1,1))
