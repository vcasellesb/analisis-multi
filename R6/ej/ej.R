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

## trying stuff about Batachartya whatever distances
pg <- matrix(c(0.21,0.06,0.06,0.67,
               0.25,0.04,0.14,0.57,
               0.22,0.06,0.08,0.64,
               0.19,0.04,0.02,0.75,
               0.18,0.00,0.15,0.67,
               0.23,0.00,0.28,0.49,
               0.30,0.00,0.06,0.64,
               0.10,0.06,0.13,0.71,
               0.27,0.04,0.06,0.63,
               0.21,0.05,0.20,0.54), ncol=4, byrow=T)
colnames(pg) <- c("gr.A","gr.B","gr.AB","gr.0")
rownames(pg) <- c("francesa","checa","germánica","vasca","china",
                  "ainu","esquimal","negra_USA","española","egipcia")

# B distance calculation
# first we need to calculate frequencies per row.
freq <- milk / 100
# Now we calculate sqrt of this
Q <- sqrt(as.matrix(freq))
D_BC <- Q %*% t(Q)
D_BC <- ifelse(D_BC>1, 1, D_BC)
D_BC <- acos(D_BC)

B <- B_from_D(sqrt(D_BC))
evecs2 <- eigen(B)$vectors[, 1:2]
evalues2 <- eigen(B)$values[1:2]
X <- evecs2 %*% diag(sqrt(evalues2))

cmdscale(sqrt(D_BC), eig = T)


Q_true <- sqrt(as.matrix(milk)/100)
BC <- Q %*% t(Q)
BC <- ifelse(BC>1., 1., BC)
D2B <- acos(BC)
