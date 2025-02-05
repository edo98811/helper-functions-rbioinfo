N <- 20000
M <- 2000
X <- matrix(rnorm(N*M),N)
system.time(crossprod(X))
