# Exercise 1

d <- matrix(c(0, 2, 3, 4.5, 4.5,
              2, 0, 4, 5, 5,
              3, 4, 0, 5, 4,
              4.5, 5, 5, 0, 2,
              4.5, 5, 4, 2, 0), nrow = 5)
row.names(d) <- c('A', 'B', 'C', 'D', 'E')

hd <- hclust(as.dist(d), method='single')
plot(hd)
dd <- as.dendrogram(hd)
wts <- c(1,2,5,10,12)
dd.reorder <- reorder(dd, wts = wts)
plot(dd.reorder)


# Exercise 2
install.packages('flexclust')
data(milk, package = "flexclust")
#milk - 
nr <- nrow(milk)
d <- matrix(0, nr, nr)
colnames(d) <- rownames(d) <- rownames(milk)
for (i in 1:(nr-1)){
  for (j in (i+1):nr){
    d[i, j] <- d[j, i] <- sqrt(sum((milk[i,] - milk[j,])**2))
  }
}
d



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

source('../../funcs/dist.R')

# we use R_dist to calculate euclidean distances
d <- R_dist(milk)
rownames(d) <- colnames(d) <- rownames(milk)

# to do MDS, I source B_from_D
source('../../funcs/MDS.R')
B <- B_from_D(d)
evalues <- eigen(B)$values

# this I assume should be the rank (5)
sum(evalues > 1e-10)
matrixcalc::matrix.rank(B) # yeah

# we get the first two evectors and evalues
evecs2 <- eigen(B)$vectors[, 1:2]
evalues2 <- diag(sqrt(eigen(B)$values[1:2]))
X <- evecs2 %*% evalues2
rownames(X) <- rownames(d)

sum(evalues[1:2]) / sum(evalues[1:5])

plot(X,type="n", xlab="PC1",ylab="PC2",xlim=c(-30,50))
text(X[,1],X[,2],labels=rownames(d),cex=0.8)
