# Studying from Everitt, 2011
# I didn't understand the bottom of page 107 (section 4.4.1)
# It ultimately boils down to me mistrusting the associative rule for sums
# and vector operations. Shameful, I know. So this is where I check it.

# Let's take an X n * m matrix 

data(crabs, package="MASS")
X <- data.matrix(crabs)

# let's subset X, since it's pretty larg
X <- X[1:20, 4:8]

# We construct B as X*Xt
B <- X%*%t(X)


rowSums(B) # we see that the sum of the first row of B is 23254.52
sum(B[1,]) # again 23254.52
sum(apply(t(X), MARGIN=2, FUN = function(x) sum(x*X[1,]))) # again, the same

# however, this is not equivalent. If v and w are vectors,
# sum(v) * sum(w) =/= sum(v*w)!!! DUMBDUMBDUMB
sapply(colSums(t(X)), FUN=function(x) sum(x*X[1,]))

# However, this is equivalent:
# w*a + w*b = w(a+b) (all are vectors)
# PROOF:
result<-c(0,0,0,0,0) # placeholder
for (i in 1:ncol(t(X))){
  result = result + t(X)[,i]
}
# In result, I have the sum of all the columns of t(X)
# If we first sum all the column vectors of t(X) (which has been done and
# the result is in the homonymous variable), multiplying afterwards by the first
# row of X we effectively construct the first row of B.
sum(X[1,] * result) == sum(B[1,]) # TRUE

###
# From Everitt 2011, section 4.4.2
# Examples of classical multidimensional scaling

X <- matrix(c(3,4,4,6,1,
              5,1,1,7,3,
              6,2,0,2,6,
              1,1,1,0,3,
              4,7,3,6,2,
              2,2,5,1,0,
              0,4,1,1,1,
              0,6,4,3,5,
              7,6,5,1,4,
              2,1,4,3,1), ncol=5, byrow=T)

D <- dist(X, diag = TRUE)
D <- as.matrix(D)

# let's check if the utility functions I've created
# to understand MDS better, I'm gonna compare their
# results to the ones I get from cmdscale (the "ground truth")
source('../../funcs/MDS.R')
B_D <- B_from_D(D)
ev <- eigen(B_D, symmetric = TRUE)
ev_values <- ev$values # "my" eigenvalues

cmds_true <- cmdscale(d=D, k=9, eig=TRUE) # true result of MDS
true_evvalues <- cmds_true$eig # "true" eigenvalues

all.equal(ev_values, true_evvalues) # evaluates to true

# if we wanna generate X with the first k=9 eigenvectors, as in cmdscale:
# NOTE: I really dislike how cmdscale is written. It's really difficult to 
# understand the code. Basically it does this:

k = 9 # this allows you to go from a m-dimensional space to a k-dimensional space
n <- nrow(D)
ev_values <- ev_values[1:k] # as I said, we only grab the k largest eigenvalues
evec <- ev$vectors[, seq_len(k), drop = FALSE] # and their corresponding eigenvectors
# Then the function essentially checks whether all eigenvalues are > 0
# Effectively just removing the ones that are not (and their eigenvectors)
evec <- evec[, ev_values > 0, drop = FALSE]
ev_values <- ev_values[ev_values>0]
# Then, they essentially generate the "new X" using the eigenvectors 
# of B (Double centering matrix of D) and the square root of the eigenvalues
# for a theoretical explanation, see Everitt's 2011 book on multivariate analysis, page ~109
points <- evec * rep(sqrt(ev_values), each = n)

# I like to do it like this:
D_1half <- diag(sqrt(ev_values))
new_x <- evec%*%D_1half

all.equal(new_x, points) # TRUE
