# Here I'm gonna play around with the following materials:
## http://cda.psych.uiuc.edu/statistical_learning_course/Jolliffe%20I.%20Principal%20Component%20Analysis%20(2ed.,%20Springer,%202002)(518s)_MVsa_.pdf

# Let's say we have some data, X
X <- matrix(rnorm(n=108), ncol = 3)

# We calculate its covariance-variance matrix and it's diagonalization
S <- cov(X)
V <- eigen(S)$vectors
D <- eigen(S)$values

D_1half <- diag(sqrt(D))
S_1half <- V%*%D_1half%*%solve(V)

# If we construct y1 as a linear combination of the columns of X, using the first
# eigenvector:
a1 <- V[,1]
y1 <- X %*% a1
t(a1)%*%S_1half%*%S_1half%*%a1

a2 <- V[,2]
y2 <- X%*%a2
t(a1)%*%S_1half%*%S_1half%*%a2

mean(y1)
a1 

# TODO: Check if a1 %*% S %*% a2 is equal to cov(a1, a2)

# What is S^(1/2)? It's a matrix where its diagonal corresponds to sd for every 
# variable and ...?
# 
##############################
# Working on Everitt 2011, Multivariate analysis smth smth
r <- 0.6
R <- matrix(c(1, r, r, 1), ncol=2)

lambda1 <- 1 + r
lambda2 <- 1 - r

a1 <- c(1/sqrt(2), 1/sqrt(2))

R%*%a1 == lambda1 * a1

# what if we change signs?
a1 <- -a1
R%*%a1 == lambda1 * a1
# evaluates to true... this is not mentioned in the book

# a2 has to check a few boxes. First, it has to be orthogonal to a1
# Secondly, it has to be a unit vector
# Third, it has to maximize the variance of a2 %*% R %*% t(a2)
a2 <- c(1/sqrt(2), -1/sqrt(2))

# first condition
t(a1) %*% a2
# second condition
sqrt(sum(a2**2))
# third condition -- actually, I don't know how to prove this...

##############################
# https://cds.nyu.edu/wp-content/uploads/2021/05/covariance_matrix.pdf
# 
S <- matrix(c(1, 0.8, 0, 0.8, 1, 0, 0,0,1.2), byrow=T, ncol=3)
# The covariance matrix encodes the variance of any linear combination of the 
# entries of a random vector.

# first recipe
v1 <- c(100, 50, 50)

# The standard deviation of v1%*%x is equal to t(v1) %*% S %*% v1
sqrt(t(v1) %*% S %*% v1)

# second recipe
v2 <- c(100, 100, 0)
sqrt(t(v2)%*%S%*%v2)

# TODO: Reproduce results from variance in specific direction part (end of section 1)

