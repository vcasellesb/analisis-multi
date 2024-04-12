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

### BREAK
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

#### CONTINUING WITH THE EXAMPLE
# it's fun how linear algebra works. Basically, we are trying to reconstruct
# a dataset X with n observations and m variables from it's n x n distance matrix D
# So, this is like saying we have n points in an m-dimensional space (X). Now,
# from D we can construct a representation in a p-dimensional that preserves D
# However, as seen just by inspecting X (from the example in Everitt's book)
# we see that it's a matrix with a rank that can be AT MOST m=5, since it's the
# number of variables we have.
# So, it makes sense that the rank of X%*%t(X) (nxn matrix) is only 5.
# Therefore, the eigenvalues != 0 that we get by spectral decomposition of X%*%t(X)
# are only 5. Proof:

library(matrixcalc)
m = 5
matrix.rank(X%*%t(X)) == m # TRUE
sum(eigen(X%*%t(X))$values > 1e-15) == m # I know I'm somewhat cherry-picking, but TRUE

D <- as.matrix(dist(X))
B <- B_from_D(D)
SD <- eigen(B)
evalues <- SD$values
evectors <- SD$vectors

# We subset the 5 eigenvectors corresponding to 5 largest eigenvalues 
evalues_5 <- evalues[1:5]
evectors_5 <- evectors[, 1:5]

X_reconstructed <- evectors_5 %*% diag(sqrt(evalues_5))
D_reconstructed <- as.matrix(dist(X_reconstructed))
max(abs(D_reconstructed - D)) # O(10^-14)

# Let's try it with 3
evalues_3 <- evalues[1:3]
evectors_3 <- evectors[, 1:3]

X_reconstructed2 <- evectors_3 %*% diag(sqrt(evalues_3))
D_reconstructed2 <- as.matrix(dist(X_reconstructed2))
max(abs(D_reconstructed2 - D)) # O(1)

# Let's try it with 4
evalues_4 <- evalues[1:4]
evectors_4 <- evectors[, 1:4]

X_reconstructed3 <- evectors_4 %*% diag(sqrt(evalues_4))
D_reconstructed3 <- as.matrix(dist(X_reconstructed3))
max(abs(D_reconstructed3 - D)) # O(1) still...

