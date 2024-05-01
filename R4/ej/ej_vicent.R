# Exercises contained in this same folder.
source('../../funcs/CA.R')

D_true <- D <- function(x) {
  a <- t(t(x) / colSums(x))
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                         a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                        sum(x) / rowSums(x)))
  matrix(ret, ncol = ncol(x))
}

# Exercise 1

dat <- matrix(c(6,1,4,2,
                1,3,25,2,
                11,11,0,20), ncol=3)

# Exercise 2
max(abs(d_chisq(dat) - D_true(dat))) # e-16

P <- rowprofile(dat)
c <- colSums(dat) / sum(dat)
D <- diag(1/sqrt(c))
max(abs(as.matrix(dist(P%*%D) ** 2) - d_chisq(dat, FALSE) ** 2))


# Observar que la distancia ji-cuadrado entre perfiles equivale a la distancia 
# euclídea entre los vectores transformados y = D−1/2p , es decir,
# entre las filas de la matriz de datos
# Checking this shit out

P <- rowprofile(dat)
Dc <- diag(1/sqrt(rowprofile(dat, T)[5,]))
y1 <- P%*%Dc
y1
Df <- diag(1/column_profiles(dat, T)[,4])
F_ <- dat/sum(dat)
max(abs(Df%*%F_- P))
max(abs(Df%*%F_%*%Dc - P%*%Dc))
# Nice

# Exercise 3
# To perform MDS we have to calculate B from the distance matrix and compute
# SVD on B. I'm gonna use the code from R3
source('../../funcs/MDS.R')
D <- d_chisq(dat, col=FALSE)
B <- B_from_D(D)
evalues <- eigen(B)$values
evecs <- eigen(B)$vectors

# Veiem que hi han dos evalues més grans que 0, podem deduir que la matriu B
# és de rank 2
library(matrixcalc)
matrix.rank(B) # 2
# Seleccionem els dos primers evalues i evecs i obtenim la nova matriu "X"
evecs2 <- evecs[, 1:2]
evals2 <- diag(sqrt(evalues[1:2]))

newX <- evecs2 %*% evals2
max(abs(D - as.matrix(d_chisq(newX, F))))

# copiau del profe
library(MASS)
eqscplot(newX,ty="n",xlab="PC1",ylab="PC2",xlim=c(-1.5,0.8))
abline(v=0,h=0, col="gray",lty=4)
text(newX[,1],newX[,2], labels=c('A1', "A2", "A3", "A4"),cex=0.8)

# Columns:
D_col <- d_chisq(dat, col=T)
B <- B_from_D(D_col)
evalues <- eigen(B)$values
evecs <- eigen(B)$vectors

evalues # it also has only 2 >0 evalues. Makes sense --> colRank == rowRank

# We construct "new X" the same way
evalues2 <- diag(sqrt(evalues[1:2]))
evecs2 <- evecs[,1:2]
newX <- evecs2 %*% evalues2
max(abs(D_col - as.matrix(d_chisq(newX, T))))
library(MASS)
eqscplot(newX,ty="n",xlab="PC1",ylab="PC2",xlim=c(-1.5,1.1))
abline(v=0,h=0, col="gray",lty=4)
text(newX[,1],newX[,2], labels=c("E1", "E2", "E3"),cex=0.8)

