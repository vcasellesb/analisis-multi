## Super naive implementation of pairwise mahalanobis distance

mah_one <- function(x, y, S){
  x <- as.numeric(x)
  y <- as.numeric(y)
  dm <- (x-y) %*% solve(S) %*% (x-y)
  dm <- sqrt(dm)
  dm
}

mah <- function(df){
  dimmy <- dim(df)[1]
  res <- matrix(0, dimmy, dimmy)
  S <- cov(df)
  for (i in 1:dimmy){
    for (j in 1:dimmy){
      if (i == j){
        res[i, j] <- 0
      }
      else{
        x <- huswif[i, ]
        y <- huswif[j, ]
        
        res[i, j] <- mah_one(x, y, S)
      }
    }
  }
  res
}