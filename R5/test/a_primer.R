# Studying from Multivariate Statistical Methods: A Primer

# load data
load('gorriones.RData')
X <- data.matrix(gorriones)
x_1hat <- colMeans(X[X[, 6] == 2, , drop=FALSE] )[-6]
x_2hat <- colMeans(X[X[, 6] == 1, ,drop=FALSE])[-6]
C1 <- cov(X[X[,6]==2, , drop=FALSE])[-6, -6]
C2 <- cov(X[X[,6]==1, , drop=FALSE])[-6, -6]
n1 <- nrow(X[X[,6]==2, , drop=FALSE])
n2 <- nrow(X) - n1
C <- ((n1 - 1) * C1 + (n2-1) * C2) / (n1 + n2 - 2)
T_squared <- n1*n2*(x_1hat - x_2hat) %*% solve(C) %*% (x_1hat - x_2hat) / (n1+n2)


# Example 4.2
# Està fatal explicat. Bàsicament, el tio diu que estandaritza les dades, però
# no especifica com. Ho fa amb les estadístiques globals, no per grup
X_scaled <- scale(X)[, -6]
# Un cop tenim les dades escalades, en substraiem la mediana -- ara si, per grup
superv <- gorriones$superviv == "S"
abs_dev_med <- function(mat, boolvec){
  m1 <- mat[boolvec,]; m2 <- mat[!boolvec, ]
  mats <- list(m1, m2)
  res <- NULL
  for (m in mats){
    medians <- apply(m, MARGIN = 2, FUN = median)
    if (is.null(res)) res <- abs(sweep(m, MARGIN=2, STATS=medians, FUN="-"))
    else res <- rbind(res, abs(sweep(m, 2, medians, "-")))
  }
  res
}
ADM <- abs_dev_med(X_scaled, superv)
ADM <- cbind(ADM, gorriones$superviv)
# Això ens dóna la matriu amb les desviacions absolutes de la mediana. He 
# afegit a la última columna el vector contenint els grups. La codificació és la següent:
# 2 --> supervivents
# 1 --> no supervivents

# A continuació reprodueixo la computació del t-statistic "de Levene" que computa
# a l'inici de l'exemple 4.2. Està fatal explicat. Bàsicament, el tio diu que fa
# la mitjana de les desviacions absolutes de la mediana. El tema és que ho realitza
# amb les dades SENSE escalar. Això s'ho calla. De totes maneres, dóna el mateix
# amb les dades escalades o sense escalar

# escric una funció per computar el t-statistic fàcilment. M'agrada programar.
# em baso amb ${anal-multiv-dir}/funcs/multivariate-stats.R pooledS
source('../../funcs/multivariate-stats.R')
levene_ttest <- function(mat, factor){
  factor <- as.numeric(factor)
  factors <- unique(factor)
  
  i <- which(colSums(sweep(mat, 1, factor, "==")) == nrow(mat)) # THIS IS SO DUMB
  if (length(i) > 0) mat <- mat[, -i, drop=FALSE]
  n_col <- ncol(mat)
  
  mean_vecs <- c()
  nis <- c()
  for (f in factors){
    subset_mat <- mat[factor==f, , drop=FALSE]
    nis <- append(nis, nrow(subset_mat))
    mean_vecs = append(mean_vecs, colMeans(subset_mat))
    
  }
  # now we parse results
  num <- mean_vecs[1:n_col] - mean_vecs[(n_col+1):(2*n_col)]
  denom <- diag(pooledS(mat, factor))
  denom <- denom * ((1/nis[1]) + (1/nis[2]))
  denom <- sqrt(denom)
  
  num / denom
}

ADM_noscale <- abs_dev_med(X, superv)[, -6]
ADM_scaled <- abs_dev_med(X_scaled, superv)
all.equal(levene_ttest(ADM_noscale, gorriones$superviv), 
          levene_ttest(ADM_scaled, gorriones$superviv)) # TRUE
