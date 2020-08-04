library(fpp2)

# exercise 2.1
load('./data/GoldenGate.RData')

gg.ts <- ts(GoldenGate$traffic, start=1968, frequency=12)
ggtsdisplay(gg.ts)

# a)
ndiffs(gg.ts)
nsdiffs(gg.ts)

gg.d1 <- diff(gg.ts, differences = 1)
ggtsdisplay(gg.d1)

# seasonal differencing 
gg.sd1 <- diff(gg.d1, differences = 1, lag = 12)
ggtsdisplay(gg.sd1)
# series looks stationary, there seem to beno seasonal pattern
# left in the data. With some more extreme values in the plot.

# b)
# Simple Exponential Smoothing
gg.ses <- HoltWinters(gg.sd1, alpha=0.5, beta=FALSE, gamma=FALSE)
gg.ses

# or use gg.ses$fitted values
plot(gg.ses)
gg.ses$SSE

# Goodness-Of-Fit measures
# In-Sample Accuracy
with(gg.ses,accuracy(fitted,x))
# With alpha=0.5, the theil's U score is
# better than optimizing by MSE.

# Residuals
# In-Sample Diagnostics
checkresiduals(gg.ses)
# The residuals seem not normally distributed
# by the histogram.
# also the ACF has multiple significant values 

# Forecast
gg.ses.forc <- forecast(gg.ses, h=10)
autoplot(gg.ses.forc)

# Box test to check Non-Zero Autocorrelations
Box.test(gg.ses.forc$residuals, lag=20)
# The Box test yields a significant value,
# There is still some structure left in the residuals

# Normality of the residuals
shapiro.test(gg.ses.forc$residuals)
# We reject the null hypothesis of the residuals
# being normally distributed.

# c) 
# Corrected only for seasonality
gg.sd2 <- diff(gg.ts, differences = 1, lag = 12)
gg.ses <- HoltWinters(gg.sd2, gamma=FALSE)
gg.ses

plot(gg.ses)
gg.ses$SSE

with(gg.ses, accuracy(fitted, x))
# presumably worse than naive methods

checkresiduals(gg.ses)
# We observe 1 major outlier which is significant
# on the same range as we applied season differencing
# Also the histogram plot doesn't seem normally distributed

gg.ses.forc <- forecast(gg.ses, h=10)
autoplot(gg.ses.forc)

Box.test(gg.ses.forc$residuals, lag=20)
shapiro.test(gg.ses.forc$residuals)
# Both significant


# d)
# Corrected only for trend
gg.ses <- HoltWinters(gg.d1, beta = FALSE)
gg.ses

gg.ses$SSE
plot(gg.ses)

with(gg.ses, accuracy(fitted, x))
# We observe a really good Theil's U score

checkresiduals(gg.ses)
# We observe 1 significant value for the ACF
# which could be due to the finite differencing

gg.ses.forc <- forecast(gg.ses, h=10)
autoplot(gg.ses.forc)

Box.test(gg.ses.forc$residuals, lag=20)
# The Box test is not significant

shapiro.test(gg.ses.forc$residuals)
# We cant assume the residuals are normally distributed


# e)
gg.ses <- HoltWinters(gg.ts)
gg.ses

gg.ses$SSE
plot(gg.ses)

with(gg.ses, accuracy(fitted, x))

checkresiduals(gg.ses)

gg.ses.forc <- forecast(gg.ses, h=10)
autoplot(gg.ses.forc)

Box.test(gg.ses.forc$residuals, lag=20)
shapiro.test(gg.ses.forc$residuals)


# exercise 2.2
# (repeat exercise 2.1)
load('./data/airline.RData')




# exercise 2.3
# a)
data(ausbeer)
ausbeer

ausb.ts <- ausbeer
autoplot(ausb.ts)

ausb.ses <- HoltWinters(ausb.ts)
ausb.ses
plot(ausb.ses)

ausb.ses$SSE

with(ausb.ses, accuracy(fitted, x))
# Good Theil's U score 

checkresiduals(ausb.ses)
# residuals with 1 significant ACF

ausb.ses.forc <- forecast(ausb.ses, h=20)
Box.test(ausb.ses.forc$residuals, lag=20, type="Ljung-Box")
# We can assume there is no structure left in the ACF

shapiro.test(ausb.ses.forc$residuals)
# We can also assume the residuals are normally distributed

# b)
ausb.ets <- ets(ausb.ts)
ausb.ets
# model fit plot
plot(ausb.ets)

# In-Sample accuracy
with(ausb.ets, accuracy(fitted, x))

# In-Sample diagnosis
checkresiduals(ausb.ets)

shapiro.test(ausb.ets$residuals)
# Residuals seem to be normally distributed

# c)
ausb.ets.forc <- forecast(ausb.ets, h=20)
autoplot(ausb.ets.forc)

# 95% Confidence intervals
ausb.ets.forc


# exercise 2.4



