# Testing out rcpp
library(rbenchmark)

f <- function(n) {
  if (n < 2) return(n)
  return(f(n-1) + f(n-2))
}
# Using it on first 11 arguments
sapply(0:10, f)

library(Rcpp)
cppFunction("
int g(int n) {
if (n < 2) return(n);
return(g(n-1) + g(n-2));
}")
# Using it on first 11 arguments
sapply(0:10, g)

benchmark(f(10), f(15), f(20))
benchmark(g(10), g(15), g(20))

benchmark(f(20), g(20))