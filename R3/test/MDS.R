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