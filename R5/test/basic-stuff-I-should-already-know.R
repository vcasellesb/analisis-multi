# Studying re multivariate distributions
# However, to understand multivariate distribution functions you should
# first understand univariate distribution functions

# A density function is a function that, when you integrate that function 
# definitely -- using an interval [a, b] -- you get the probability of x
# (the variable from the density function) falling on that interval


# e.g. the density function of a standard normal distribution is given 
# here
f_normal_standard <- function(x){
  num <- exp(-(x^2)/2)
  denom <- sqrt(2*pi)
  num/denom
}

# if you integrate using the 97.5 and 2.5 percentiles, you get 
integrate(f_normal_standard, -1.96, 1.96) # 0.95 (as you'd expect)
integrate(f_normal_standard, -Inf, 1.96) # 0.975
integrate(f_normal_standard, -Inf, -1.96) # should be 0.025 (it is)
integrate(f_normal_standard, -Inf, 1.645) # 0.95
integrate(f_normal_standard, -Inf, Inf) # 1

# Let's try out another function
f <- function(x){
  cond <- (x > 2) | (x < 0)
  if (cond) return(0)
  else return(1/2)
}

int_f <- function(x){
  x / 2
}

################################################################
# Studying from: https://www.stat.uchicago.edu/~stigler/Stat244/ch3withfigs.pdf
# Let's define our function:
f_x_y <- function(x,y) {
  y * (1/2 - x) + x
}
fy <- function(y){
  1/2 * y
}

integrate(fy, lower=0, upper=2)
x <- seq(0, 1, length=100)
y <- seq(0,2,length=100)
z <- outer(x,y,f_x_y)
persp(x, y, z,
      theta=30, phi=20,
      r=50, d=0.1, expand=0.5,
      ltheta=90, lphi=180,
      shade=0.75,
      ticktype="detailed",
      cex.axis=0.7,
      zlab="density"
)

f_x_cond_y <- function(x, y){
  y * (1 - 2*x) + 2*x
}
x <- seq(0, 1, length=100)
y <- 1/2
z <- f_x_cond_y(x,y)
plot(x, z, type='l', ylim = c(0, 2))

# area should be 1
b = 1 
h = 3/2 - 1/2
b * h / 2 + 1/2 * 1

################################################################
# Example 3C
x <- runif(100)
for (x_i in x){
  y <- runif(100, min=0, max=x_i)
  print('mean y with x:')
  print(x_i)
  print(mean(y))
}

f_y_given_x <- function(x){
  1 / x
}

y <- seq(0, 1/4, length=100)
plot(y, rep(f_y_given_x(1/4), 100), type='l', ylim=c(0, 6))

# Once again, we see that this conditional distribution function does not have area 1 when
# plotting it using R
# It's a rectangle with base 1 and height 0.3333...
1 * 4 # not 1

