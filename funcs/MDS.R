## Trying to understand MDS via:
# https://github.com/mariarizzo/energy/blob/master/src/centering.cpp
# src/library/stats/src/dblcen.c from: https://github.com/SurajGupta/r-source

doublecenter <- function(D){
  # This is literally copied from 
  # https://github.com/mariarizzo/energy/blob/master/src/centering.cpp {lines 16 to 45}
  
  # Parameters:
  #   D: square n x n matrix
  n <- ncol(D)
  A <- matrix(0, ncol=n, nrow=n)
  akbar <- c()
  abar <- 0
  
  for (k in 1:n){
    akbar[k] = 0
    for (j in 1:n){
      akbar[k] = akbar[k] + D[k, j]
    }
    abar = abar + akbar[k]
    akbar[k] = akbar[k] / n
  }
  abar = abar/(n**2)
  
  for (k in 1:n){
    for (j in k:n){
      A[k, j] <- D[k, j] - akbar[k] - akbar[j] + abar
      A[j, k] <- A[k, j]
    }
  }
  A
}
