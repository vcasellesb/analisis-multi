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

R_dist <- function(x){
  nc <- ncol(x); nr <- nrow(x)

  x <- unname(unlist(c(x))) # convert it to a vector by column
  res <- matrix(0, nr, nr)
  for (i1 in 1:(nr-1)){
    for (i2 in (i1+1):nr){
      res[i1, i2] <- res[i2, i1] <- R_euclid(x, nc, nr, i1, i2)
    }
  }
  res
}
R_dist(milk)
