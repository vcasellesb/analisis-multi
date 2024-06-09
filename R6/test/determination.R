# I am determined to learn what this bitch is doing:
# https://github.com/SurajGupta/r-source/blob/a28e609e72ed7c47f6ddfbb86c85279a0750f0b7/src/library/stats/src/hclust.f#L42

# So, I'm gonna adapt the fortran code to be able to run on R

IOFFST <- function(n, i, j){
  # J+(I-1)*N-(I*(I+1))/2
  j + (i-1) * n - (i*(i+1))/2
}

hclust_true <- function(n, len, iopt, ia, ib, crit, 
                        member, nn, disnn, flag, diss){
  # lines 53 to 61
  for (i in 1:n){
    flag[i] <- TRUE
  }
  ncl <- n
  
  # lines 74 to 85
  for (i in 1:(n-1)){
    dmin <- Inf
    for (j in (i+1):n){
      ind <- IOFFST(n, i, j)
      if (dmin > diss[ind]){
        dmin <- diss[ind]
        jm <- j
      }
    }
    nn[i] <- jm
    disnn[i] <- dmin
  }
  
  while(ncl > 1) {
    # lines 91 to 99
    dmin <- Inf
    for (i in 1:(n-1)){
      if (flag[i] & (disnn[i] < dmin)){
        dmin <- disnn[i]
        im <- i
        jm <- nn[i]
      }
    }
    ncl <- ncl - 1
    
    # lines 103 to 111
    i2 <- min(im, jm)
    j2 <- max(im, jm)
    ia[n - ncl] <- i2
    ib[n - ncl] <- j2
    crit[n-ncl] <- dmin
    flag[j2] <- FALSE
    
    # lines 115 to 183
    dmin <- Inf
    for (k in 1:n){
      if (flag[k] & k!=i2){
        if (i2 < k){
          ind1 <- IOFFST(n, i2, k)
        }
        else ind1 <- IOFFST(n, k, i2)
        if (j2 < k){
          ind2 <- IOFFST(n, j2, k)
        }
        else ind2 <- IOFFST(n, k, j2)
        
        diss[ind1] <- min(diss[ind1], diss[ind2])
        if (i2 < k){
          if(diss[ind1]<dmin){
            dmin<-diss[ind1]
            jj <- k
          }
        }
        else {
          if (diss[ind1] < disnn[k]){
            disnn[k] <- diss[ind1]
            nn[k] <- i2
          }
        }
      }
    }
    member[i2] <- member[i2] + member[j2]
    disnn[i2] <- dmin
    nn[i2] <- jj
    
    # lines 187 to 204
    for (i in 1:(n-1)){
      if (flag[i] & ((nn[i]==i2) | (nn[i] == j2))){
        dmin <- Inf
        for (j in (i+1):n){
          if (flag[j]){
            ind <- IOFFST(n, i, j)
            if (diss[ind] < dmin){
              dmin <- diss[ind]
              jj <- j
            }
          }
        }
        nn[i] <- jj
        disnn[i] <- dmin
      }
    }
  }
  list(diss=diss, nn=nn, member=member, ia=ia, ib=ib, crit=crit)
}

# function call
d <- as.dist(dumdum)
n <- as.integer(attr(d, "Size"))
len <- as.integer(n*(n-1)/2)
member <- rep(1, n)

hcl <- hclust_true(
  n = n,
  len=len,
  ia=integer(n),
  ib=integer(n),
  crit=double(n),
  member=as.double(member),
  nn=integer(n),
  disnn=double(n),
  flag=logical(n),
  diss=d
)


hcass2_true <- function(n, ia, ib, iorder, iia, iib){
  for (i in 1:n){
    iia[i] <- ia[i]
    iib[i] <- ib[i]
  }
  
  for(i in 1:(n-2)){
    k=min(ia[i],ib[i])
    for (j in (i+1):(n-1)){
      if (ia[j] == k) iia[j] <- -i
      if (ib[j] == k) iib[j] <- -i
    }
  }
  
  for (i in 1:(n-1)){
    iia[i] <- -iia[i]
    iib[i] <- -iib[i]
  }
  
  for (i in 1:(n-1)){
    if (iia[i] > 0 & iib[i] < 0){
      k <- iia[i]
      iia[i] <- iib[i]
      iib[i] <- k
    }
    if (iia[i]>0 & iib[i]>0){
      k1 <- min(iia[i], iib[i])
      k2 <- max(iia[i], iib[i])
      iia[i]<-k1; iib[i]<-k2
    }
  }
  
  # ORDER
  iorder[1] <- iia[n-1]
  iorder[2] <- iib[n-1]
  loc = 2
  for (i in seq(from=n-2, to=1, by=-1)){
    for (j in 1:loc){
      if (iorder[j] == i){
        iorder[j]<-iia[i]
        if (j==loc){
          loc <- loc+1
          iorder[loc] <- iib[i]
        }
        else{
          loc <- loc + 1
          for (k in seq(from=loc, to=j+2, by=-1)){
            iorder[k] <- iorder[k-1]
          }
          iorder[j+1] <- iib[i]
        }
      }
    }
  }
  
  for (i in 1:n){
    iorder[i] <- -iorder[i]
  }
  
  list(order=iorder, iia=iia, iib=iib)
}

hcass <- hcass2_true(
  n=n,
  ia = hcl$ia,
  ib = hcl$ib,
  iorder = integer(n),
  iia = integer(n),
  iib = integer(n)
)

match.call.wrapper <- function(f){
  
}

x <-structure(list(merge = cbind(hcass$iia[1L:(n-1)], hcass$iib[1L:(n-1)]),
               height = hcl$crit[1L:(n-1)],
               order = hcass$order,
               labels = attr(d, "Labels"),
               method = 'single',
               call = match.call(),
               dist.method = attr(d, "method")),
          class = "hclust")
