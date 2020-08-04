library(fpp2)
set.seed(42)

# Generate White Noise processes
## random normal variates
wn.norm1 <- rnorm(n = 100, mean = 5, sd = 0.2)
wn.norm2 <- rnorm(n = 100, mean = 5, sd = 10)
## random Poisson variates
wn.pois <- rpois(n = 100, lambda = 20)

grid.arrange(autoplot(ts(wn.norm1)), 
             autoplot(ts(wn.norm2)), 
             autoplot(ts(wn.pois)))

grid.arrange(ggAcf(wn.norm1),
             ggAcf(wn.norm2),
             ggAcf(wn.pois))
             
acf(wn.pois)

# Generate Random Walk process
## initialize {x_t} and {w_t}
rw <- rnorm(n = 100, mean = 0, sd = 1)
rw.ts <- ts(cumsum(ww))

grid.arrange(autoplot(rw.ts), 
             ggAcf(rw.ts))

# Generate sine wave
n <- 100
sw.ts1 <- ts(sin(2 * pi * seq(n)/20))

grid.arrange(autoplot(sw.ts1),
  ggAcf(sw.ts1, lag.max = 30))

# Adding a linear trend to the sine wave
sw.ts2 <- ts(sin(2 * pi * seq(n)/12) + seq(n)/30)

grid.arrange(autoplot(sw.ts2),
             ggAcf(sw.ts2, lag.max = 30))




