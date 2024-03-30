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


# Strang Problem Set I.3, page 20
# Show that the nullspace of AB contains thenull space of B.If Bx=0 then...

# Let's say we have a matrix A with rank r and dimensions m = 4 and n = 3
# it has rank r = 2
A <- matrix(c(1, 2, 2, 2,
              3, 4, 5, 5,
              2, 4, 4, 4), ncol=3)

# Now let's create a matrix B, with m = 3, n = 3, r = 2
B <- 
# TODO