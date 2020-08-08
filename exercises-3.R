library(fpp2)
library(gridExtra)
library(nortest)


# exercise 3.1
# Stochastic process 1;
# X_t = Z_t + 0.7 * Z_t-1

# a)
ts.stoch1 <-arima.sim(list(order=c(0,0,1), ma=0.7), n=100)
autoplot(ts.stoch1)

# Theoretical ACF and PACF
ACF.theor <- ARMAacf(ma=c(0.7), lag.max=20)
plot(ACF.theor)
abline(h=0)

PACF.theor <- ARMAacf(ma=c(0.7), lag.max=20, pacf=TRUE)
plot(PACF.theor)
abline(h=0)

# Stochastic process 2:
# X_t = Z_t + 0.7 * Z_t-1 - 0.2 * Z_t-2

# a)
ts.stoch2 <-arima.sim(list(order=c(0,0,2), ma=c(0.7,-0.2)),n=100)
autoplot(ts.stoch2)

# Theoretical ACF and PACF
ACF.theor <- ARMAacf(ma=c(0.7,-0.2), lag.max=20)
plot(ACF.theor)
abline(h=0)

PACF.theor <- ARMAacf(ma=c(0.7,-0.2), lag.max=20, pacf=TRUE)
plot(PACF.theor)
abline(h=0)


# exercise 3.2

# X_t = Z_t + C(Z_t-1 + Z_t-2 + ...)
# a)
# This can be written as MA(infinity) or AR(1) model.


# exercise 3.3
# 1. X_t = X_t-1 + Z_t
# AR(1) model with alpha = 1
e3.ts1 <- arima.sim(list(order=c(1,0,0), ar=c(1.0)), n=250)
ggtsdisplay(e3.ts1)

# Invertible and/or stationary?
# The model is not stationary by definition
# as the alpha coefficient is 1.

# 2. X_t = Z_t - 0.5 Z_t-1 - 0.1 Z_t-2
# MA(2) model with beta = 0.5, 0.1
e3.ts2 <- arima.sim(list(order=c(0,0,2), ma=c(0.5, 0.1)), n=250)
ggtsdisplay(e3.ts2)

# Invertible and/or stationary?
polyroot(c(1, 0.5, -0.1))
# Which shows us all roots are outside the complex unit circle.
# Thus its invertible.

# 3.X_t = Z_t + 1.1 Z_t-1
# MA(1) model with beta = 1.1
e3.ts3 <- arima.sim(list(order=c(0,0,1), ma=c(1.1)), n=250)
ggtsdisplay(e3.ts3)

# Invertible and/or stationary?
# Which leads to B = -1/1.1, as the root is in the complex
# unit circle.

# 4.X_t = 1.1 X_t-1 + Z_t
# AR(1) model with alpha = 1.1
e3.ts4 <- arima.sim(list(order=c(1,0,0), ar=c(1.1)), n=250)
ggtsdisplay(e3.ts4)

# Invertible and/or stationary?
# This model is not stationary, alpha > 1.1

# 5. X_t = 1.1 X_t-1 - 0.5 X_t-2 + Z_t
e3.ts5 <- arima.sim(list(order=c(2,0,0), ar=c(1.1, 0.5)), n=250)
ggtsdisplay(e3.ts5)

# Invertible and/or stationary?
# This model is not stationary as,
polyroot(c(1, 1.1, 0.5))

# 6. X_t = 0.5 X_t-1 + Z_t - 1.3 Z_t-1 + 0.4 Z_t-2
# ARMA(1, 2) model with alpha = 0.5, beta = 1.3 and 0.4
e3.ts6 <- arima.sim(list(order=c(1,0,2), ar=c(0.5), ma=c(1.3, 0.4)), n=250)
ggtsdisplay(e3.ts6)

# Invertible and/or stationary?
# rewrite the stochastic process,
# X_t - 0.5 X_t-1 = Z_t - 1.3 Z_t-1 + 0.4 Z_t-2
polyroot(c(1, -0.5))
# Thus the AR component is stationary
polyroot(c(1,-1.3, 0.4))
# The MA component seems to have roots outside the 
# complex unit circle.
auto.arima(e3.ts6)
autoplot(auto.arima(e3.ts6))


# exericse 3.4
# a)
# X_t = 0.75 X_t-1 + 0.25 X_t-2 + Z_t
# AR(2) model with alpha = 0.75, 0.25

# b) Is the series stationary?
polyroot(c(1,-0.75, -0.25))
# Is not stationary as one of the roots is inside the complex
# unit circle.

# c)
e4.ts <- arima.sim(list(order=c(2,0,0), ar=c(0.75, 0.25)), n=250)
ggtsdisplay(e4.ts)


# exercise 3.5
# a)
# X_t = Z_t + 0.75 Z_t-1 + 0.25 Z_t-2
# MA(2) model with betas = 0.75, 0.25

# b) Is the series invertible?
polyroot(c(1, 0.75, 0.25))
# Yes, the roots are outside the complex unit circle

# c)
e5.ts <- arima.sim(list(order=c(0,0,2), ma=c(0.75, 0.25)), n=250)
ggtsdisplay(e5.ts)
# Has 2 significant contributions for the ACF, which seems 
# reasonable given the MA(2) process.


# Test the MA(2) model
e5.arma <- arima(e5.ts, order=c(0, 0, 2))
e5.arma
autoplot(e5.arma)


# exercise 3.6
# a)
e6.ts1 <- arima.sim(model=list(order=c(2,0,0), ar=c(0.5, 0.25)), 
                    n=250)
e6.ts2 <- arima.sim(model=list(order=c(0,0,3), ma=c(0.5, 0.5, 0.5)), 
                    n=250)
e6.ts3 <- arima.sim(model=list(order=c(2,0,3), ar=c(0.5, 0.25),
                    ma=c(0.5, 0.25, 0.25)),
                    n=250)

# exercise 3.7
# a)
e7.ts1 <- arima.sim(model=list(order=c(2,0,1), ma=c(0.5), 
                               ar=c(0.5, 0.25)), n=250)
e7.ts2 <- arima.sim(model=list(order=c(2,1,1),
                               ar=c(0.5, 0.25), 
                               ma=c(0.5)), n=250)
e7.ts3 <- arima.sim(model=list(order=c(1,2,1), 
                               ar=c(0.5),
                               ma=c(0.5)), n=250)
e7.ts4 <- arima.sim(model=list(order=c(1,1,2), 
                               ar=c(0.5),
                               ma=c(0.5, 0.25)), n=250)


# b) 
# Compare with exercise 3.6
# 3.6, model 1,
grid.arrange(autoplot(e6.ts1), autoplot(e7.ts1),
             autoplot(e7.ts2), autoplot(e7.ts3), 
             autoplot(e7.ts4))


# 3.6, model 2,
grid.arrange(autoplot(e6.ts2), autoplot(e7.ts1),
             autoplot(e7.ts2), autoplot(e7.ts3), 
             autoplot(e7.ts4))


# 3.6, model 3,
grid.arrange(autoplot(e6.ts3), autoplot(e7.ts1),
             autoplot(e7.ts2), autoplot(e7.ts3), 
             autoplot(e7.ts4))


# exercise 3.10
# X_t = (1+0.75B)(1+0.5B^6)Z_t

# a)
# Can be identified as a SARIMA(0, 0, 1)x(0, 0, 1)_6
# model. 

# b)
e10 <- Arima(ts(rnorm(250),freq=4), 
               order=c(0,0,1), 
               seasonal=c(0,0,1),
               fixed=c(Theta=0.5, theta=0.75, S=6))

# Simulate
e10.sim <- simulate(model, nsim=1000)
ggtsdisplay(e10.sim)


# exercise 3.13


