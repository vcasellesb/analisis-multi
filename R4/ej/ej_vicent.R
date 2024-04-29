# Exercises contained in this same folder.
source('../../funcs/CA.R')

D_true <- D <- function(x) {
  a <- t(t(x) / colSums(x))
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                         a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                        sum(x) / rowSums(x)))
  matrix(ret, ncol = ncol(x))
}

# Exercise 1

dat <- matrix(c(6,1,4,2,
                1,3,25,2,
                11,11,0,20), ncol=3)

# Exercise 2
max(abs(d_chisq(dat) - D_true(dat))) # e-16

P <- rowprofile(dat)
c <- colSums(dat) / sum(dat)
D <- diag(1/sqrt(c))
max(abs(as.matrix(dist(P%*%D) ** 2) - d_chisq(dat, FALSE) ** 2))
