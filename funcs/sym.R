# Functions to check matrix properties, especially relating to symmetric matrices

pos_def_1 <- function(A){all(eigen(A)$values > 0)}

pos_def_2 <- function(A){
  R <- chol(A)
  all(diag(R) > 0)
  }

positive_definite <- function(m){
  ## yet another way... the determinant of R has to be nonzero
  R <- try(chol(m), silent = T)
  if (is.matrix(R)){
    if (det(R) != 0){return(T)}
  }
  else{return(F)}
  return(F)
}