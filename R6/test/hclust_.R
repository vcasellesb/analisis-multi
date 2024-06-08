# Cluster analysis. I studied from Multivariate Statistical Methods A Primer

dumdum <- matrix(c(0, 2, 6, 10, 9,
                   2, 0, 5, 9, 8,
                   6, 5, 0, 4, 5,
                   10, 9, 4, 0, 3,
                   9, 8, 5, 3, 0), ncol=5, nrow=5)
d <- as.dist(dumdum)
as.integer(attr(d, "Size"))

get_nn_and_distnn <- function(d){
  n <- dim(d)[1] # we assume d is symmetric
  nn <- vector(length=n)
  disnn <- vector(length=n)
  for (i in 1:n){
    neighbors_i <- d[i, ][d[i,] != 0]
    jj <- which.min(neighbors_i)
    j <- get_index_dist_mat(jj, i)
    nn[i] <- j
    disnn[i] <- d[i, j]
  }
  list(NN = nn, DISNN = disnn)
}

get_index_dist_mat <- function(j, i){
  if (j >= i){
    return(j + 1)
  }
  else j
}

# https://github.com/mortcanty/CRCDocker/blob/97d791c131beaaa9b9d4d189d5f61a0384af16f7/src/mlpy-3.5.0/mlpy/hcluster/hc.c#L41
#ioffst <- function(n, i, j)
#{
#  j + i * n - (i + 1) * (i + 2) / 2;
#}
# ioffst(5, 1, 2)

get_nn_and_distnn(dumdum)

dmatrix2 <- matrix(c(0,1,2,4,5,
                     1,0,2.5,4,5,
                     2,2.5,0,4,5,
                     4,4,4,0,1.2,
                     5,5,5,1.2,0),nrow=5)

get_nn_and_distnn(dmatrix2)

iterate_d <- function(d, i, j){
  # first copy d
  dim1 <- dim(d)[1]
  d_new <- matrix(0, nrow=dim1-1, ncol =dim1-1)
  d_new[]
}
