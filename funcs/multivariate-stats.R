

# test
pooledS <- function(X, fac)
{
  n <- nrow(X)
  if (!is.numeric(fac)) fac <- as.numeric(fac)
  groups <- unique(fac)
  k <- length(groups)
  Sp <- 0
  
  i <- which(colSums(sweep(X, 1, fac, "==")) == nrow(X)) # THIS IS SO DUMB
  
  for (g in groups){
    Xi <- X[fac == g, , drop=FALSE]
    if (length(i) > 0) Xi <- Xi[, -i, drop=FALSE]
    Si <- cov(Xi)
    ni <- nrow(Xi)
    Sp <- Sp + (ni - 1) * Si
  }
  Sp <- Sp * 1 / (n - k)
  Sp
}

levene_ttest <- function(mat, factor){
  factor <- as.numeric(factor)
  factors <- unique(factor)
  
  i <- which(colSums(sweep(mat, 1, factor, "==")) == nrow(mat)) # THIS IS SO DUMB
  if (length(i) > 0) mat <- mat[, -i, drop=FALSE]
  n_col <- ncol(mat)
  
  mean_vecs <- c()
  nis <- c()
  for (f in factors){
    subset_mat <- mat[factor==f, , drop=FALSE]
    nis <- append(nis, nrow(subset_mat))
    mean_vecs = append(mean_vecs, colMeans(subset_mat))
    
  }
  # now we parse results
  num <- mean_vecs[1:n_col] - mean_vecs[(n_col+1):(2*n_col)]
  denom <- diag(pooledS(mat, factor))
  denom <- denom * ((1/nis[1]) + (1/nis[2]))
  denom <- sqrt(denom)
  
  num / denom
}