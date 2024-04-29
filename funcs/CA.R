# Functions to solve exercises

column_profiles <- function(mat, average=FALSE){
  res <-t(t(mat) / colSums(mat))
  if (average) {
    average_col <- rowSums(mat) / sum(mat)
    res <- cbind(res, average_col, deparse.level = 0)
    if (!is.null(colnames(mat))) colnames(res) <- c(colnames(mat), 'average')
  }
  res
}

rowprofile <- function(mat, average=FALSE){
  res <- mat / rowSums(mat)
  if (average){
    average_row <- colSums(mat) / sum(mat)
    res <- rbind(res, average_row, deparse.level=0)
    if (!is.null(rownames(mat))) rownames(res) <- c(rownames(mat), 'average')
  }
  return(res)
}

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