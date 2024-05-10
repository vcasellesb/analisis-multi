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
