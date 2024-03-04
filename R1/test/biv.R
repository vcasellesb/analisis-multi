x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:10; names(y) <- paste(y,":", sep = "")
outer(y, x, `^`)

outer(month.abb, 1999:2003, FUN = paste)

## three way multiplication table:
x %o% x %o% y[1:3]

mx <- mean(x)
sdx <- sqrt(var(x))
my <- mean(y)
sdy <- sqrt(var(y))
x <- scale(x)
y <- scale(y)
ngridx <- 30
ngridy <- 30
den <- matrix(0, ngridx, ngridy)
n <- length(x)
hx <- 1 * n^(-0.2)
hy <- 1 * n^(-0.2)
h <- hx*hy
hsqrt <- sqrt(h)
seqx <- seq(range(x)[1], range(x)[2], length = ngridx)
seqy <- seq(range(y)[1], range(y)[2], length = ngridy)
X <- x[1]
Y <- y[1]
xx <- (seqx - X)/hsqrt
yy <- (seqy - Y)/hsqrt
den <- den + outer(xx, yy, function(x, y)
  exp(-0.5 * (x^2 + y^2)))
den <- den/(n * 2 * pi * h)
seqx <- sdx * seqx + mx

bivden<-function(x, y, ngridx = 30, ngridy = 30, constant.x = 1, constant.y = 1) {
  #x and y are vectors containing the bivariate data
  #ngridx and ngridy are the number of points in the grid
  #
  mx <- mean(x)
  sdx <- sqrt(var(x))
  my <- mean(y)
  sdy <- sqrt(var(y))
  #scale x and y before estimation
  x <- scale(x)
  y <- scale(y)
  #
  den <- matrix(0, ngridx, ngridy)
  #
  #find possible value for bandwidth
  #
  n <- length(x)
  #
  hx <- constant.x * n^(-0.2)
  hy <- constant.y * n^(-0.2)
  h <- hx * hy
  hsqrt <- sqrt(h)
  #
  seqx <- seq(range(x)[1], range(x)[2], length = ngridx)
  seqy <- seq(range(y)[1], range(y)[2], length = ngridy)
  #
  for(i in 1:n) {
    X <- x[i]
    Y <- y[i]
    xx <- (seqx - X)/hsqrt
    yy <- (seqy - Y)/hsqrt
    den <- den + outer(xx, yy, function(x, y)
      exp(-0.5 * (x^2 + y^2)))
  }
  den <- den/(n * 2 * pi * h)
  seqx <- sdx * seqx + mx
  seqy <- sdy * seqy + my
  result <- list(seqx = seqx, seqy = seqy, den = den)
  result
}

univarden <- function(x, seqx=50, constant.x=1){
  # we get what we need, i.e. mean, sd, bandwith(h)
  mx <- mean(x)
  sdx <- sqrt(var(x))
  n <- length(x)
  range1 = range(x)[1]; range2 = range(x)[2]
  
  # values of x we will estimate den for
  seqx_vec <- seq(range1, range2, length=seqx)
  h <- 0.79 * IQR(x) * n^(-0.2) # from Węglarczyk 2018
  
  den <- numeric(length=seqx)
  
  for (i in 1:n){
    X <- x[i]
    # it's the same seqx_vec - X or the other way
    xx <- (X-seqx_vec)/h
    den <- den + exp(-0.5 * (xx^2))
  }
  
  den <- den/(n*sqrt(2*pi)*h)
  result <- list(seqx = seqx_vec, den=den)
  result
}
