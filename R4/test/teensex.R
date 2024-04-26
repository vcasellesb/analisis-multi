cols <- c("<16", "16-17", "17-18", "18-19", "19-20")
rows <- c("No boyfriend", "Boyfriend no sex", "Boyfriend sex")
dat <- matrix(c(21,8,2,
                 21,9,3,
                 14,6,4,
                 13,8,10,
                 8,2,10), ncol= length(cols), nrow=length(rows))

colnames(dat) <- cols
rownames(dat) <- rows

d_chisq <- function(mat, col = TRUE,
                    return_profile = FALSE){
  # I made it easily costumizable, 
  # if you want to use column profiles
  # set col = TRUE (if you wanna use row profiles
  # set it to FALSE -- duh).
  
  mat <- if (col) mat else t(mat) # taken from everitt 2011
  it <- if (col) ncol else nrow
  
  n <- it(mat)
  res <- matrix(0, ncol = n, nrow = n)
  
  f_weights <- if (col) rowSums else colSums
  weights <- sum(mat) / f_weights(mat)
  
  props <- t(t(mat) / colSums(mat)) # taken from everitt 2011
  if (return_profile){
    return(props) # if you just wanna get the profiles
  }
  
  # chisq distance calculation
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      v1 <- props[,i]
      v2 <- props[,j]
      subs <- (v1 - v2)**2
      resy <- sum(subs * weights)
      res[i,j] <- res[j,i] <- sqrt(resy)
    }
  }
  res
}


D_true <- D <- function(x) {
  a <- t(t(x) / colSums(x))
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                         a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                        sum(x) / rowSums(x)))
  matrix(ret, ncol = ncol(x))
}
all.equal(D_true(dat), d_chisq(dat)) # TRUE


