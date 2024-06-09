## Super naive implementation of pairwise mahalanobis distance

mah_one <- function(x, y, solved_S){
  x <- as.numeric(x)
  y <- as.numeric(y)
  dm <- (x-y) %*% solved_S %*% (x-y)
  dm <- sqrt(dm)
  dm
}

mah <- function(df){
  n <- nrow(df)
  res <- matrix(0, n, n)
  S <- cov(df)
  solved_S <- solve(S)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      res[i, j] <- mah_one(df[i,], df[j,], solved_S)
      res[j, i] <- res[i, j]
    }
  }
  res
}

# tests
data(crabs, package='MASS')
df <- crabs
df <- df[, 4:8, drop=FALSE]

mah_prova <- mah(df) # super slow

differences <- c()
i <- 0
for (row in 1:nrow(df)){
  for (row2 in row:nrow(df)){
    true <- mahalanobis(as.vector(df[row,], mode="numeric"),
                        as.vector(df[row2,], mode="numeric"), cov(df))
    i = i + 1
    differences[i] = abs(mah_prova[row, row2] - sqrt(true))
  }
}
max(differences) # in my laptop, this is O(10^-15)

# I got inspo from 
# https://github.com/wch/r-source/blob/e5b21d0397c607883ff25cca379687b86933d730/src/library/stats/src/distance.c

R_euclid <- function(x, nc, nr, i1, i2){
  
  res <- 0
  for (j in 1:nc){
    dev <- (x[i1] - x[i2])
    res <- res + dev**2
    i1 <- i1 + nr
    i2 <- i2 + nr
  }
  sqrt(res)
}

R_dist <- function(x, method='euclid'){
  nc <- ncol(x); nr <- nrow(x)
  
  dist_func <- switch(
    method,
    'euclid' = R_euclid
    )
  
  x <- unname(unlist(c(x))) # convert it to a vector by column
  res <- matrix(0, nr, nr)
  for (i1 in 1:(nr-1)){
    for (i2 in (i1+1):nr){
      res[i1, i2] <- res[i2, i1] <- dist_func(x, nc, nr, i1, i2)
    }
  }
  res
}

# tests
# all(unname(as.matrix(dist(milk, 'euclidean'))) == R_dist(milk, 'euclid'))
