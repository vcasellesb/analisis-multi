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

A <- A <- matrix(c(8,1,6, 3,5,7,
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

check_it <- function(m){
  ## yet another way... the determinant of R has to be nonzero
  R <- try(chol(m), silent = T)
  if (is.matrix(R)){
    if (det(R) != 0){return(T)}
  }
  else{return(F)}
  return(F)
}

check_it(A)
