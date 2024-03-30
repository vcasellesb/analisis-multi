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

# What is S^(1/2)?

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

eigen(R)

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

##############################
# Everitt 2011: Section 3.10 (Examples -- reproduction)

# EXAMPLE 1
source('headsize.R')
head(headsize) # haha
# Veiem que hi han 4 variables

# A l'exemple només s'utilitzen les columnes head1 i head2
head_dat <- headsize[, c(1, 3)]

head_pca <- princomp(x = head_dat) # True pca

S <- cov(head_dat)
A <- eigen(S)$vectors
D <- eigen(S)$values

a1 <- A[, 1]
t(a1) %*% S %*% a1 - D[1]
y1 <- head_dat %*% a1

a2 <- A[, 2]
t(a2) %*% S %*% a2 - D[2]
y2 <- head_dat %*% a2

# Podem reproduir la matriu de covariances amb la 
# matriu generada escalant els eigenvectors amb sqrt(eigenvalues)
a1_ast <- a1 * sqrt(D[1])
a2_ast <- a2 * sqrt(D[2])
A_ast <- matrix(c(a1_ast, a2_ast), ncol=2)

A_ast %*% t(A_ast)
S

# As an exercise, readers might like to find the predicted covariance 
# matrix using only the first component.
# això entenc que es faria així
a1_ast %*% t(a1_ast)
