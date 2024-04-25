cols <- c("<16", "16-17", "17-18", "18-19", "19-20")
rows <- c("No boyfriend", "Boyfriend no sex", "Boyfriend sex")
dat <- matrix(c(21,8,2,
                 21,9,3,
                 14,6,4,
                 13,8,10,
                 8,2,10), ncol= length(cols), nrow=length(rows))

colnames(dat) <- cols
rownames(dat) <- rows

column_prop <- t(t(dat) / colSums(dat))
row_prop <- t(t(dat) / rowSums(dat))

sqrt(sum((column_prop[, 1] - column_prop[, 2])**2))


d_chisq_col <- function(mat){
  n <- ncol(mat)
  res <- matrix(0, n, n)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      
    }
  }
  res
}

D <- function(x) {
  a <- t(t(x) / colSums(x))
  print(a)
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                       a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                      sum(x) / rowSums(x))) 
  r <- matrix(ret, ncol = ncol(x))}

D(dat)
  