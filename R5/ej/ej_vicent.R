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

## ground truth (teacher's implementation)
rnorm2 <- function(n,mu1,mu2,s1,s2,rho){
 x1 <- rnorm(n, mu1, s1)
 x2 <- rnorm(n, mu2 + rho*s2*(x1-mu1)/s1, s2*sqrt(1-rho^2))
 cbind(x1, x2)
}
set.seed(1234)
smpl <- rnorm2(500,0,5,2,3,0.5)
all(X == smpl) # TRUE
