library(fpp2)
library(gridExtra)
library(nortest)


# exercise 4.1
load('./data/ARMAsimulations.RData')
names(ARMA.simulations)

# ARMA1 process
# a)
e1.ts <-ts(ARMA.simulations$ARMA1)
ggtsdisplay(e1.ts)
# Could indicate an AR(1) model by looking at PACF

# Spectrum plot
ggtsdisplay(e1.ts, plot.type="spectrum")
spec.pgram(e1.ts, span = 5)

# b)
# possibility of AR(1) model
# data seems, to be stationary

ndiffs(e1.ts)
# Doesn't seem to need any finite differencing

# c)
e1.sim <-arima.sim(list(order=c(1,0,0), ar=0.5), n=100)
e1.arma <- Arima(e1.ts, order=c(0,0,1))

grid.arrange(autoplot(e1.sim), autoplot(e1.ts))

# verify parameters
e1.arma
abs(0.79/0.02) > 2 # Thus has impact
abs(-0.1291/0.2203) > 2 # Thus the intercept is not significant

# In-Sample accuracy
with(e1.arma, accuracy(fitted, x))

# In-Sample residuals
checkresiduals(e1.arma)
# The Box test seem to be significant there
# is no structure left.
shapiro.test(e1.arma$residuals)
# The residuals seem to be normally distributed
autoplot(e1.arma)
# The root is in the complex unit circle

# Model forecast
e1.arma.fore <- forecast(e1.arma,h=19)
autoplot(e1.arma.fore)

# d)
# X_t Phi_1(B) = Z_t


# ARMA2 process
# a)
e2.ts <- ts(ARMA.simulations$ARMA2)
ggtsdisplay(e2.ts)
# A significant contribution at PACF 1,
# Where the ACF is decaying downwards.

# b)
# Given the PACF this looks like a AR(1) model.
# there is no trend or seasonality in the data.
e2.ar1 <- Arima(e2.ts, order=c(1, 0, 0))
e2.auto <- auto.arima(e2.ts)
e2.ar1
e2.auto

# parameter significance,
abs(-0.7873/-0.0443) > 2 # ar1, significant
abs(-0.0443/0.0254) > 2 # intercept, insignificant

# In-Sample Diagnostics
checkresiduals(e2.ar1)

# Significant
Box.test(e2.ar1$residuals, type="Ljung-Box")
shapiro.test(e2.ar1$residuals)
autoplot(e2.ar1)

# In-Sample Accuracy
with(e2.ar1, accuracy(fitted, x))
# Auto obtains the same model

# ARMA9 process
e9.ts <- ARMA.simulations$ARMA9
ggtsdisplay(e9.ts)
# Streng correlation in ACF and a trend in the data.
e9.d1 <- diff(e9.ts, differences = 1)
ggtsdisplay(e9.d1)

# Both these models seem possible, but compared to the
# auto arima it adds a lot of parameters which are
# insignificant and additional model complexity.
e9.ar013 <- Arima(e9.ts, order=c(0, 1, 3))
e9.ar013

e9.ar310 <- Arima(e9.ts, order=c(3, 1, 0))
e9.ar310

e9.auto <- auto.arima(e9.ts)
e9.auto

# All residuals seem to follow the assumptions.
checkresiduals(e9.ar013)
checkresiduals(e9.ar310)
checkresiduals(e9.auto)


# exercise 4.2
load('./data/GoldenGate.RData')

# a)
gg.ts <- ts(GoldenGate$traffic, start=c(1968, 1), frequency=12)
ggtsdisplay(gg.ts)



# TODO: excercise 4.5