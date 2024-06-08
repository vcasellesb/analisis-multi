## Exercici 1

cols <- c("A", "B", "C")
rows <- paste0("P", 1:6, sep='')
dt <- matrix(c(0.130, 0.11, 0.09,
               0.105, 0.065, 0.08,
               0.07, 0.035, 0.045,
               0.08, 0.04, 0.08,
               0.01, 0.02, 0.02,
               0.005, 0.01, 0.005), ncol=3, byrow=T)
colnames(dt) <- cols
rownames(dt) <- rows
rowSums(dt) # marginal distributions of rows
colSums(dt) # marginal distributions of cols

# conditional probabilities given columns
t(t(dt) / colSums(dt))

## Exercici 5

# generem el que es demana
n <- 10000
x1 <- rnorm(n)
x2 <- rnorm(n)
X <- cbind(x1, x2)

# c)
y1_ <- function(x1, mu1, sigma1){
  mu1 + sigma1 * x1
}

y2_ <- function(x1, x2, mu2, sigma2, rho){
  mu2 + sigma2 * (rho*x1 + sqrt(1-rho**2)*x2)
}

# parameter definition
mu <- c(2, 1)
sigma1 <- 1
sigma2 <- 1.5
rho <- 0.6
Y <- cbind(y1_(X[,1], mu[1], sigma1), y2_(X[,1], X[,2], mu[2], sigma2,rho))

colMeans(Y)


N2 <- X
transform <- function(x,mu1,mu2,s1,s2,rho){
   y1 <- mu1 + s1*x[1]
 y2 <- mu2 + s2*rho*x[1] + s2*sqrt(1-rho^2)*x[2]
 c(y1,y2)
}
N2Y <- t(apply(N2,1,transform,mu1=2,mu2=1,s1=1,s2=1.5,rho=0.6))  
colnames(N2Y) <- c("y1","y2")
plot(N2Y, pch=19, col=rgb(1,0,0,alpha = 0.3))
max(abs(Y - N2Y))

# Exercise 6
n <- 500
set.seed(1234)
mu1 <- 0
mu2 <- 5
sigma1 <- 2
sigma2 <- 3
rho <- 0.5

x1 <- rnorm(n, mean = mu1, sd = sigma1)

E_Y_given_X <- function(x, mu2, sigma1, sigma2, rho, mu1){
  mu2 + (1/sigma1) * sigma2 * rho * (x - mu1)
}

E <- E_Y_given_X(x1, mu2, sigma1, sigma2, rho, mu1)
var_Y_given_X<-function(sigma2, rho){
  (sigma2**2) * (1-rho**2)
}
var <- var_Y_given_X(sigma2, rho)
s2 <- sigma2
s2*sqrt(1-rho^2) == sqrt(var)
 
# I didn't know you could pass a vector as the mean parameter for rnorm
y<-c()
for (i in 1:n){
  y <- append(y, values=rnorm(1, mean=E[i], sd=sqrt(var)))
}

X <- cbind(x1, y)
colMeans(X)
cov(X)
## ground truth (teacher's implementation)
rnorm2 <- function(n,mu1,mu2,s1,s2,rho){
 x1 <- rnorm(n, mu1, s1)
 x2 <- rnorm(n, mu2 + rho*s2*(x1-mu1)/s1, s2*sqrt(1-rho^2))
 cbind(x1, x2)
}
set.seed(1234)
smpl <- rnorm2(500,0,5,2,3,0.5)
all(X == smpl) # TRUE

# sourcing file with bivden function
source('../../R1/test/biv.R')
den <- bivden(x=X[,1], y=X[, 2])
persp(den$seqx, den$seqy, den$den, xlab="x1 " ,
       ylab="x2" ,
       zlab="density",lwd=2, theta=50)
plot(X[, 1],X[, 2])
contour(den$seqx,den$seqy,den$den,lwd=2,nlevels=20,add=T)

# Again, teacher's solution
den1 <- bivden(smpl[,1], smpl[,2])
persp(den1$seqx, den1$seqy, den1$den, xlab="X1", ylab="X2", zlab="Density",
      lwd=2, ticktype="detailed", theta=50)

# c)
plot(X[, 1],X[, 2])
contour(den$seqx,den$seqy,den$den,lwd=2,nlevels=25,add=T)

# Exercise 7
# Let's define the density function of a bivariate normal distribution with
# mean vector \mu and covariance matrix \Sigma
cov2cor_vc <- function(cov){ # from:
  # https://math.stackexchange.com/questions/186959/correlation-matrix-from-covariance-mat
  D <- diag(sqrt(diag(cov)))
  cor <- solve(D) %*% cov %*% solve(D) 
  cor
}

mu1 <- 1
mu2 <- 2
sigma1 <- sqrt(4)
sigma2 <- sqrt(9)
rho <- 0.5
x <- seq(from=mu1 - sigma1 * 4, to=mu1 + 4*sigma1, by=0.1)
y <- seq(from=mu2 - sigma2 * 4, to=mu2 + sigma2 * 4, by=0.1)
fxy <- function(x,y,mu1,mu2,sigma1, sigma2, rho){

  Q <- ((x-mu1)^2/sigma1^2
        - 2*rho*(x-mu1)*(y-mu2)/(sigma1*sigma2)
        + (y-mu2)^2/sigma2^2)/(1-rho^2)
  1/(2*pi*sigma1*sigma2*sqrt(1-rho^2)) * exp(-Q/2)
}
z <- outer(X=x, Y=y, fxy, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

persp(x, y, z, col="lightgreen", theta=30, phi=20, r=50, d=0.1, expand=0.5, 
      ltheta=90, lphi=180, shade=0.75, cex.axis=0.7, ticktype="detailed")

# ground truth
mu1<-1 
mu2<-2
s11<-4
s12<-3
s22<-9
rho<-s12/sqrt(s11*s22)
x1 <- seq(mu1-4*sqrt(s11), mu1+4*sqrt(s11), by=0.1)
x2 <- seq(mu2-4*sqrt(s22), mu2+4*sqrt(s22), by=0.1)
f<-function(x1,x2){
  Q <- (x1-mu1)^2/s11 - 2*rho*(x1-mu1)*(x2-mu2)/sqrt(s11*s22) + (x2-mu2)^2/s22
  1/(2*pi*sqrt(s11*s22*(1-rho^2)))*exp(-Q/(2*(1-rho^2)))
}
z_profe <- outer(x1,x2,f)
all(z == z_profe) # TRUE


# Testing geometric series
# this shouldn't be here
# it's from https://www.stat.uchicago.edu/~stigler/Stat244/ch3withfigs.pdf
geom <- function(start=1, p = 0.5, inf = 100000){
  seqit <- seq(start, inf)
  res <- 0
  for (x in seqit){
    tmp <- (x+1) * p * (1-p)**(x-1)
    res <- tmp + res
  }
  res
}

inf1 = geom(start=2, p=0.5)
inf2 = geom(start=1, p=0.5)
inf3 = geom(start=1, p=0.5)


generate_samples <- function(){
  x <- runif(n=100000)
  y <- runif(n=100000, min=0, max=x)
  
  cbind(x,y)
}

x_y <- generate_samples()
