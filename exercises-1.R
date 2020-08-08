library(fpp2)
library(gridExtra)
library(nortest)

# exercise 1.1
# a)
# Perform EDA
load('./data/airline.RData')
airline.ts <- ts(airline$passenger, start=c(1949,1), end=c(1960,1), frequency=12)
autoplot(airline.ts)

# b, d & e)
# ACF and PACF
ggtsdisplay(airline.ts)

ndiffs(airline.ts) # suggested 1 differences
airline.diff1 <- diff(airline.ts, differences=1)
autoplot(airline.diff1)
ggtsdisplay(airline.diff1)

nsdiffs(airline.ts) # Seasonal differencing, 1
airline.sdiff1 <- diff(airline.ts, differences=1, frequency=12)
autoplot(airline.sdiff1)
ggtsdisplay(airline.sdiff1)
# Does not look like random noise, the variance is growing

# c & f)
# Check if an additive decomp is adquete for the model
airline.decomp <- decompose(airline.ts, type="additive")
names(airline.decomp)

# If the variance in the residuals seem to increase or decrease
# overtime, an multiplicative model seems more suitable.
autoplot(airline.decomp$random)

# More suitable in this case
airline.decomp <- decompose(airline.ts, type="multiplicative")
autoplot(airline.decomp$random)

ggtsdisplay(airline.decomp$random)


# exercise 1.2
# a)
ggtsdisplay(airline.ts)

pacf(airline.ts, lag.max = 24)
acf(airline.ts, lag.max = 12)

mean(acf(airline.ts, lag.max = 12)$acf)
mean(pacf(airline.ts, lag.max = 12)$acf)

# b)
# Finite differencing
ggtsdisplay(airline.diff1)

# Finite and seasonal differencing
ggtsdisplay(airline.sdiff1)

# c)
## Data filtering/Smoothing
# MA transformation
airline.ma3 <- filter(airline.ts, filter=rep(1/3,3), sides=2)
airline.ma9 <- filter(airline.ts, filter=rep(1/9,9), sides=2)

tsplot <- autoplot(airline.ts)
ma3 <- autoplot(airline.ma3)
ma9 <- autoplot(airline.ma9)

grid.arrange(tsplot, ma3, ma9, ncol = 3)

# EWMA Smoothing
autoplot(smooth(airline.ts, kind = "3RSS"))

# Spectrum transformation, peridiogram
spec.pgram(airline.ts, span = 10)

# Variance stabilization
# Log transform
grid.arrange(
  autoplot(airline.ts), autoplot(log(airline.ts)))

airline.log <- log(airline.ts)
ggtsdisplay(airline.log, plot.type=c("histogram"))
shapiro.test(airline.log)


# BoxCox transform
BoxCox.lambda(airline.ts)
airline.BC <- BoxCox(airline.ts, lambda=BoxCox.lambda(airline.ts))
ggtsdisplay(airline.BC, plot.type=c("histogram"))
shapiro.test(airline.BC)
# We reject the H_0 the data is not normally distributed.

ad.test(airline.BC)
# Same for Anderson-Darling

# QQ plot
qqnorm(airline.BC)


# exercise 1.3
load('./data/ARMAsimulations.RData')

# a)
arma.ts1 <- ts(ARMA.simulations$ARMA1)
arma.ts2 <- ts(ARMA.simulations$ARMA2)
arma.ts3 <- ts(ARMA.simulations$ARMA3)

grid.arrange(autoplot(arma.ts1), autoplot(arma.ts2), autoplot(arma.ts3))

# b)
ggtsdisplay(arma.ts1)
ggtsdisplay(arma.ts2)
ggtsdisplay(arma.ts3)

# c)
spec.pgram(arma.ts1, span = 10)
spec.pgram(arma.ts2, span = 10)
spec.pgram(arma.ts3, span = 10)


# exercise 1.4
load("./data/skirts.RData")

z <- arima.sim(model = list(order = c(0, 1, 0)), n=400, mean=0.5,sd=2)
ggtsdisplay(ts(z))
# A corresponds to 1

# B corresponds to 4,
# a fast changing seismic wave

z <- decompose(airline.ts)
ggtsdisplay(z$seasonal)
# D corresponds to 3

skirt.ts <- ts(skirts, start=1866, frequency=1)
autoplot(skirt.ts)
ggtsdisplay(skirt.ts)
# C corresponds to 2,
# as example look at the skirts plot


# exercise 1.5
z <- arima.sim(model = list(order = c(0, 1, 0)), n=400, mean=0.5,sd=2)
z.ts <- ts(z)
z.pg <- spec.pgram(z.ts, span = 10)
ggtsdisplay(z.pg$freq)

y <- decompose(airline.ts)
y.pg <- spec.pgram(y$seasonal)
ggtsdisplay(log1p(y.pg$freq))


# exercise 1.6
airline.decomp <- decompose(airline.ts)
ggtsdisplay(airline.decomp$trend)

ggtsdisplay(ts(ausbeer))

# TS1 corresponds to AC2
# TS2 corresponds to AC3
# TS3 corresponds to AC1