## Provant d'implementar Gmedian

Gmedian_vc <- function(X, gamma=2, alpha=0.75, nstart=2, epsilon=1e-8){
  
  n <- nrow(X)
  p <- ncol(X)
  
  medvec <- X[1, ]
  medrm <- X[1, ]
  
  for (nbcomp in 1:nstart){
    for (it in 2:n){
      normxm <- sqrt(sum((X[it,] - medrm) ** 2))
      if (normxm > epsilon){
        poids <- sqrt(p)*gamma*((it+1)**(-alpha)) / normxm
        medrm = medrm + poids * (X[it, ] - medrm)
      }
      medvec = medvec + (medrm - medvec) / (it+1)
    }
  }
  medvec
}

geometric_median_vc <- function(X, tol = 1e-10, maxiter = 1000)
{
  x.old <- colMeans(X)
  for (i in 1:maxiter){
    norm <- apply(X, 1, function(x) sqrt(sum((x - x.old)**2 )))
    x.new <- colSums(X/norm) / sum(1/norm)
    diff <- max(abs(x.new - x.old))
    if (diff < tol){
      break
    }
    x.old <- x.new
  }
  x.new   
}

medtrue <- geometric_median(X)
medvc <- geometric_median_vc(X)
mednotrue <- Gmedian_vc(X)

dists <- 0
for (i in 1:nrow(X)){
  dists <- dists + sqrt(sum((medvc - X[i,])**2))
}

dists2 <- 0
for (i in 1:nrow(X)){
  dists2 <- dists2 + sqrt(sum((medtrue - X[i,])**2))
}

dists3 <- 0
for (i in 1:nrow(X)){
  dists3 <- dists3 + sqrt(sum((mednotrue - X[i,])**2))
}


x.old <- colMeans(X)
norm<-sqrt(rowSums(sweep(X, 2, x.old, "-") ** 2))
norm2 <- apply(X, 1, function(x) sqrt(sum((x - colMeans(X))**2 )))


colSums(sweep(X, 1, norm, "/")) / sum(1/norm)

colSums(X / norm)

max(abs(sweep(X, 1, norm, "/") - X / norm))

# Function
U <- X
u.old <- colMeans(U)
norm <- sqrt(rowSums(sweep(U, 2, u.old, "-")^2))
u.new <- colSums(sweep(U, 1, norm, "/"))/sum(1/norm)
diff <- max(abs(u.new - u.old))

x.old <- colMeans(X)

norm2 <- apply(X, 1, function(x) sqrt(sum((x - colMeans(X))**2 )))
x.new <- colSums(X/norm2) / sum(1/norm2)
diff2 <- max(abs(x.new - x.old))

diff == diff2
sum(norm != norm2)

# THEN WHY THE FUCK DOES IT NOT CONVERGE!