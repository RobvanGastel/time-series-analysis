library(fpp2)
library(gridExtra)
library(nortest)


# exericse 5.1, 5.2
# are proofs


# exercise 5.3
load('./data/catalog.RData')

# a)
# Careful as order matters.
names(catalog)
wom.ts <- ts(catalog$women)
men.ts <- ts(catalog$men)
autoplot(cbind(wom.ts, men.ts))

ggtsdisplay(wom.ts)
# Seasonality in the data at time-lag 12.
# And there seems to be some trend in the data.
# significance at time-lag 1 for ACF and PACF

ggtsdisplay(men.ts)
# Seasonality in the data at time-lag 12, and trend.
# singificance at time-lag 1 and 2 for ACF and PACF.

# b & c)
# Cross-correlation
ggCcf(wom.ts, men.ts, na.action=na.omit)
ggCcf(men.ts, wom.ts, na.action=na.omit)

# creating stationary series by differencing,
wom.d1 <- diff(wom.ts)
men.d1 <- diff(men.ts)
ggCcf(wom.d1, men.d1, na.action=na.omit)
# there seems to be some cyclic behaviour in the data

# Removing seasonality,
wom.sd1 <- diff(wom.d1, lag=12)
men.sd1 <- diff(men.d1, lag=12)
ggCcf(wom.sd1, men.sd1, na.action=na.omit)
# There still seems to be some cyclic structure left,
# but here doesn't seem to be a clear sine wave visible.


# exercise 5.4
load('./data/viscosity.RData')
names(viscosity)

# a)
temp.ts <- ts(viscosity$temp)
visc.ts <- ts(viscosity$visc)
autoplot(cbind(temp.ts, visc.ts))

ggtsdisplay(temp.ts)
ggtsdisplay(visc.ts)

# Cross-correlation
ggCcf(temp.ts, visc.ts)


# b)
# Dynamic regression fit
visc.proxy <- Arima(visc.ts, 
                    xreg=temp.ts,order=c(2,0,0))
visc.proxy

# In-Sample Diagnostics
checkresiduals(visc.proxy)

# In-Sample Accuracy
with(visc.proxy, accuracy(fitted, x))


# Automatic Dynamic regression
visc.auto <- auto.arima(visc.ts, xreg=temp.ts)
visc.auto

# In-Sample Diagnostics
checkresiduals(visc.auto)

# In-Sample Accuracy
with(visc.auto, accuracy(fitted, x))


