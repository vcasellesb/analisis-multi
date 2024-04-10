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
  A * (-1/2)
}

doublecenter(D)
all(energy::D_center(D) == doublecenter(D))

B_from_D <- function(D){
  # Parameters:
  #   D: square n x n matrix
  D_squared = D**2
  n <- nrow(D)
  B <- matrix(0, ncol=n, nrow=n)
  
  d_dot_dot <- sum(D_squared)/(n**2)
  for (i in 1:n){
    di_dot <- sum(D_squared[i,])/n
    for (j in i:n){
      dj_dot <- sum(D_squared[,j])/n
      B[i, j] = -(1/2) * (D_squared[i,j] - di_dot - dj_dot + d_dot_dot)
      B[j, i] = B[i, j]
    }
  }
  B
}

all(B_from_D(D) == doublecenter(D**2)) # TRUE!
