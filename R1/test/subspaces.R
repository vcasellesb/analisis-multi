# Strang and https://math.mit.edu/~gs/linearalgebra/ila5/linearalgebra5_10-1.pdf
A = matrix(
  c(-1,-1,0,-1,0,0,
    1,0,-1,0,-1,0,
    0,1,1,0,0,-1,
    0,0,0,1,1,1),
  ncol=4)

# There should be 3 soluctions to A^t * y = 0
y1 <- c(0,0,1,0,-1,1)

t(A)%*%y1
sol <- matrix(c(y1, 
                0,-1,-1,1,1,-2,
                1,0,0,-1,1,0), ncol=3)

t(A)%*% sol # all solutions
det(t(A)%*%A)

sol2 <- matrix(c(y1, 
                0,-1,-1,1,1,-2,
                1,-1,1,0,0,0), ncol=3)
