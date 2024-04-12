## Super naive implementation of pairwise mahalanobis distance

mah_one <- function(x, y, S){
  x <- as.numeric(x)
  y <- as.numeric(y)
  dm <- (x-y) %*% solve(S) %*% (x-y)
  dm <- sqrt(dm)
  dm
}

mah <- function(df){
  n <- nrow(df)
  res <- matrix(0, n, n)
  S <- cov(df)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      x <- df[i, ]
      y <- df[j, ]
      res[i, j] <- mah_one(x, y, S)
      res[j, i] <- res[i, j]
    }
  }
  res
}

# tests
data(crabs, package='MASS')
df <- crabs
df <- df[, 4:8, drop=FALSE]
all.equal(mahalanobis(df, center), mah(df))
mah_prova <- mah(df)

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
