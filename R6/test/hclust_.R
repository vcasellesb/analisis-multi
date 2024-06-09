# Cluster analysis. I studied from Multivariate Statistical Methods A Primer

dumdum <- matrix(c(0, 2, 6, 10, 9,
                   2, 0, 5, 9, 8,
                   6, 5, 0, 4, 5,
                   10, 9, 4, 0, 3,
                   9, 8, 5, 3, 0), ncol=5, nrow=5)
d <- as.dist(dumdum)
as.integer(attr(d, "Size"))

hclust_v <- function(d){
  # d is distance matrix (n x n). ds have to be squared
  n <- nrow(d)
  
  # initialize variables
  flag <- rep(TRUE, n)
  crit <- numeric(n)
  ncl <- n
  ia <- numeric(n); ib <- numeric(n)
  members <- rep(1, n)
  
  # create list of nn and nn
  res <- get_nn_and_distnn(d, n)
  nn <- res$nn; disnn <- res$disnn
  
  while(ncl > 1){

    res <- agglomerate(ia = ia, ib = ib, ncl = ncl,n = n, 
                       crit=crit, flag=flag, disnn=disnn)
    # parse results
    i2 <- res$i2; j2 <- res$j2; ia <- res$ia; ib<-res$ib <- crit<-res$crit; 
    flag<-res$flag; ncl <- res$ncl
    
    res <- update_clust_diss(d = d, i2 = i2, j2 = j2, n=n, disnn=disnn, nn=nn, 
                             flag=flag, members=members)
    # parse results
    d <- res$d; nn <- res$nn; disnn <- res$disnn; members <- res$members
    
    res <- update_nn(n, flag, i2, j2, d, nn, disnn)
    nn <- res$nn; disnn <- res$disnn
    
    print('nn:')
    print(nn)
    
    print('disnn:')
    print(disnn)
    
    print('distance matrix:')
    print(d)
  }
  
  list(nn=nn, disnn=disnn, d=d, members=members, crit=crit)
} 

get_nn_and_distnn <- function(d, n){
  nn <- vector(length=n)
  disnn <- vector(length=n)
  for (i in 1:(n-1)){
    neighbors_i <- d[i, ][d[i,] != 0]
    jj <- which.min(neighbors_i)
    j <- get_index_dist_mat(jj, i)
    nn[i] <- j
    disnn[i] <- d[i, j]
  }
  list(nn = nn, disnn = disnn)
}

get_index_dist_mat <- function(j, i){
  if (j >= i){
    return(j + 1)
  }
  else j
}

agglomerate <- function(ia, ib, ncl, n, crit, flag, disnn){
  dmin <- Inf
  for (i in 1:(n-1)){
    if (flag[i] & dmin > disnn[i]){
      dmin <- disnn[i]
      im <- i
      jm <- nn[i]
    }
  }
  ncl <- ncl-1
  
  i2 <- min(im, jm)
  j2 <- max(im, jm)
  ia[n - ncl] <- i2
  ib[n - ncl] <- j2
  
  crit[n-ncl] <- dmin
  
  flag[j2] <- FALSE
  
  # retorno
  list(i2=i2, j2=j2, ia=ia, ib=ib, crit=crit, flag=flag, ncl=ncl)
}

update_clust_diss <- function(d, i2, j2, n, disnn, nn, flag, members){
  dmin <- Inf
  for (k in 1:n){
    if (flag[k] & k != i2){
      d[i2, k] <- d[k, i2] <- min(d[i2, k], d[j2, k])
      if (d[i2, k] < dmin){
        dmin <- d[i2, k]
        jj <- k
      }
    }
  }
  members[i2] <- members[i2] + members[j2]
  disnn[i2] <- dmin
  nn[i2] <- jj
  list(d=d, disnn=disnn, nn=nn, members=members)
}



update_nn <- function(n, flag, i2, j2, d, nn, disnn){
  for (i in 1:(n-1)){
    if (flag[i] & ((nn[i] == i2) | (nn[i] == j2))){
      dmin<-Inf
      for (j in (i+1):n){
        if (flag[j]){
          if (d[i, j] < dmin){
            dmin <- d[i, j]
            jj <- j
          }
        }
      }
      nn[i] <- jj
      disnn[i] <- dmin
    }
  }
  list(nn=nn, disnn=disnn)
}
hclust_v(dumdum)

res <- get_nn_and_distnn(dumdum)
nn <- res$NN; distnn <- res$DISNN
n <- attr(as.dist(dumdum), "Size")
res <- least_diss(nn, distnn, n)
im <- res$im; jm <- res$jm
ia <- integer(n)
ib <- integer(n)
ncl <- n - 1
crit <- integer(n)

res <- agglomerate(im, jm, ia, ib, ncl, n, crit)
i2 <- res[[1]] ; j2 <- res[[2]]

update_clust_diss(dumdum, i2 = i2, j2=j2, n = n, distnn=distnn, nn=nn)




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
# https://github.com/mortcanty/CRCDocker/blob/97d791c131beaaa9b9d4d189d5f61a0384af16f7/src/mlpy-3.5.0/mlpy/hcluster/hc.c#L41
#ioffst <- function(n, i, j)
#{
#  j + i * n - (i + 1) * (i + 2) / 2;
#}
# ioffst(5, 1, 2)
