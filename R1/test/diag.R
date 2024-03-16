## Here I'm just gonna test some shit re matrix diagonalization

m <- matrix(c(6,0,1,3,-2,0,-8,0,-3), ncol=3)

# it has non-zero rank
det(m)

# m - \lambda * I when \lambda is 3 also has non-zero rank
det(m - 3 * diag(nrow=3, ncol=3))

e <- eigen(m)
lambda <- e$values
for (l in lambda){
  stopifnot(det(m - l * diag(nrow=3, ncol=3)) == 0)
  # we see that the eigenvalues are the values lambda
  # that make the determinant of m - \lambda * I be zero
}

############################################################

A <- matrix(c(8,1,6,3,5,7,
                   4,9,2) ,ncol=3, byrow=T)
#non-singular
det(A)

e <- eigen(A)
# there are 3 different eigenvalues
e$values

D <- diag(e$values)
V <- e$vectors

# we can recover A by diagonalization
stopifnot((V %*% D %*% solve(V)) == A)

# Is positive (semi-)definite?
c<-chol(A)

############################################################

A <- matrix(c(10,7,9,7,6,8,9,8,13), ncol=3, byrow=T)

# it's symmetric!
all(t(A) == A)

# it's also positive-definite. I've been studying how to 'easily' prove this
# but the options are limited. One way is to check that all the eigenvalues
# are >0. That's actually what the is.positive.definite function from does 
# matrixcalc does...
library(matrixcalc)
all(eigen(A)$values > 0)
is.positive.definite(A)

# but I don't like it...
# there are other ways, which are interesting and can be seen here: 
# https://math.stackexchange.com/questions/87528/a-practical-way-to-check-if-a-matrix-is-positive-definite

# we could also check if rii of matrix R so that A = t(R) %*% R (Cholesky decomposition) are all positive
R <- chol(A)
all(diag(R) > 0)

positive_definite <- function(m){
  ## yet another way... the determinant of R has to be nonzero
  R <- try(chol(m), silent = T)
  if (is.matrix(R)){
    if (det(R) != 0){return(T)}
  }
  else{return(F)}
  return(F)
}

positive_definite(A)

############################################################

# testing out rank and colspace stuff

A = matrix(c(1,3,0,1),ncol=2); B = matrix(c(2,6,4,5), ncol=2, byrow=F)
det(A%*%B)

############################################################
A <- matrix(c(-1,-1,0,0,0,
              1,0,-1,-1,0,
              0,1,1,0,-1,
              0,0,0,1,1), ncol=4)

# Null space of At (plane in R^5)
y1<-c(1,-1,3,-2,2)
y2<-c(0,0,2,-2,2)
t(A) %*% y1
t(A) %*% y2

############################################################

A <- matrix(c(1,3,2,2), ncol=2)
V <- eigen(A)$vectors; D <- diag(eigen(A)$values)
V%*%D^2%*%solve(V)

############################################################

e <- matrix(c(2,1,1/3,1,1,-1,1/3,-1,4),ncol=3)

V <- eigen(e)$vectors; D <- diag(eigen(e)$values)

e1half <- V%*%diag(1/sqrt(diag(D)))%*%solve(V)

############################################################

A <- matrix(c(1,1,2,2,2,4), nrow=2, byrow=T)

svd(A)

############################################################
# I'm very fascinated about idempotent matrices. Here is an example:

n <- 5
I = diag(n)
J = matrix(rep(1, n*n), ncol=n)
H <- I - 1/n * J

# H is idempotent. This means the following
H%*%H
H
max(abs(H%*%H - H))
# Are the same

# Also, H as defined allows to scale (substract the mean) 
# From a data matrix. For example
X = matrix(rnorm(n*n), ncol=n)
max(abs(scale(X, scale=F) - H%*%X)) 

# Also something I love is the following:
max(abs(t(H%*%X)%*%H%*%X - t(X)%*%H%*%X))
max(abs(t(X)%*%H%*%X/(n-1) - cov(X)))
# Cool

############################################################
A <- matrix(c(1,4,3,7,5,2,6,8,2), ncol=3) #
B <- matrix(c(1,2,3,2,4,5,2,4,6), ncol=3) # rank two matrix
A%*%B # obv AB is a combination of the columns of A by the rows of B
# of course, since the rows of B are not linearly independent,
# the """independentness""" of the cols of A is """lost"""
