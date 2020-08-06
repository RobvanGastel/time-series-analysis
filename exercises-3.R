library(fpp2)

# exercise 3.1
# Stochastic process 1;
# X_t = Z_t + 0.7 * Z_t-1

# a)
ts.stoch1 <-arima.sim(list(order=c(0,0,1), ma=0.7), n=100)
autoplot(ts.stoch1)

# b & c)



# Stochastic process 2:
# X_t = Z_t + 0.7 * Z_t-1 - 0.2 * Z_t-2

# a)
ts.stoch2 <-arima.sim(list(order=c(0,0,2), ma=c(0.7,0.2)),n=100)
autoplot(ts.stoch2)



