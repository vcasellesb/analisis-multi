# Discriminant analysis using mahalanobis distance

predict_lda <- function(x, X, group){
  X <- as.matrix(X)
  group <- as.factor(as.character(group))
  x <- as.vector(as.numeric(x))
  nlev <- nlevels(group)
  lev <- levels(group)
  
  S_minus_1 <- solve(cov(X))
  
  Ds <- list()
  for (l in 1:nlev){
    X_ <- X[group == lev[l], ]
    x_hat <- colMeans(X_)
    tmp <- as.numeric(t(x - x_hat) %*% S_minus_1 %*% (x - x_hat))
    Ds[[l]] <- tmp
  }
  
  g <- which.min(unlist(Ds))
  
  return(lev[g])
}

data(flea, package='GGally')
flea <- flea[-70, 1:4]
x <- flea[70, -1]
predict_lda(x=x, X=flea[, -1], group=flea[-70,1])

one_m <- matrix(rep(1, m), ncol=1)
u_sim <- t(flea[,-1]) %*% one_m * 1/m

X <- flea[, -1]
u_sim_t <- 1/m *( t(one_m) %*% as.matrix(X))


generate_H <- function(m){
  # m is an arbitrary dimension
  H <- matrix(-1/m, m, m)
  diag(H) <- rep(1, m) + diag(H)
  H
}

center_data_matrix <- function(data_matrix){
  if (!is.matrix(data_matrix)) data_matrix <- as.matrix(data_matrix)
  m <- nrow(data_matrix)
  H <- generate_H(m)
  centered <- H %*% data_matrix
  
  centered
}

X_hat <- center_data_matrix(flea[,-1])
t(X_hat) %*% X_hat * 1/(72)

## Example from https://www.cs.umb.edu/~dsim/slidesFISH.pdf

X1 <- matrix(c(4, 7,
               4, 13, 
               3, 17,
               7, 11,
               10, 13,
               16, 17), ncol=2, byrow = T)

X2 <- matrix(c(13, 7,
               16, 10,
               20, 11,
               23, 11,
               22, 13,
               25, 13), ncol=2, byrow=T)

colmeans <- function(X){
  m <- nrow(X); n <- ncol(X)
  u <- 1/m * rep(1, m) %*% X
  u
}

u1_sim <- colmeans(X1); u2_sim <- colmeans(X2)
X1_hat <- center_data_matrix(X1)
X2_hat <- center_data_matrix(X2)

SX2 <- t(X2_hat) %*% X2_hat
SX1 <- t(X1_hat) %*% X1_hat
Sw <- SX1 + SX2

u <- u1_sim -  u2_sim

Sb <- t(u) %*% u
d <- solve(Sw) %*% t(u)
# Aquest vector d, si no m'equivoco, et dona la projecció al nou 
# espai en una dimensionalitat inferior i que permet separar les dues classes
y1 <- X1%*%d
y2 <- X2%*%d

lda_two_classes <- function(X, group){
  X <- as.matrix(X)
  
  f1 <- levels(group)[1]
  X1 <- X[group == f1, ]; X2 <- X[group != f1, ]
  
  u1_sim <- colMeans(X1); u2_sim <- colMeans(X2)
  X1_hat <- center_data_matrix(X1)
  X2_hat <- center_data_matrix(X2)
  
  SX2 <- t(X2_hat) %*% X2_hat
  SX1 <- t(X1_hat) %*% X1_hat
  Sw <- SX1 + SX2
  
  u <- u1_sim -  u2_sim

  Sb <- t(u) %*% u
  d <- solve(Sw) %*% u
  
  return(d)
}
data(flea, package="GGally")
flea <- flea[(flea$species == species1 | flea$species == species2), 1:4]

lda_two_classes(flea[,-1], flea$species)

MASS::lda(flea[,-1], droplevels(flea$species))

# Everitt 2005
source('skulls.R')

k <- 5 # number of classes
m <- nrow(skulls)
n <- ncol(skulls[, -1])

separate_df <- function(df, f){
  n <- nlevels(f)
  l <- levels(f)
  splitted <- list()
  for (i in 1:n){
    splitted[[l[i]]] <- data.matrix(df[f == l[i], -1])
  }
  splitted
}

separated_skulls <- separate_df(skulls, skulls$EPOCH)

total_variance_helper <- function(x, colmeans){
  sweep(x, 2, STATS=colmeans, FUN = "-")
}

within_variance_helper <- function(x){
  colmeans <- colMeans(x)
  sweep(x, 2, STATS=colmeans, FUN='-')
}

substracted <- lapply(separated_skulls, FUN = total_variance_helper, colmeans=colMeans(skulls[, -1]))

T_ <- matrix(0, 4, 4)

for (j in 1:k){
  tmp <- substracted[[j]]
  T_ <- T_ + t(tmp) %*% tmp
}

W_ <- matrix(0, 4, 4)
substracted_W <- lapply(separated_skulls, FUN = within_variance_helper)
for (j in 1:k){
  tmp <- substracted_W[[j]]
  W_ <- W_ + t(tmp)%*%tmp
}

B_ <- T_ - W_

eigen(solve(W_) %*% B_)

x <- as.matrix(flea[, -1])
grouping <- flea$species
g <- as.factor(droplevels(grouping))
p <- ncol(x)
group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
f1 <- sqrt(diag(var(x - group.means[g,  ])))
