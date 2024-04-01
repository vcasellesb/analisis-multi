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
B <- matrix(c(1, 3, 3,
              6, -1 , 2,
              2, 6, 6), ncol=3)

rankMatrix(A) == rankMatrix(B) # both are rank 2

# A%*%B should also be rank two
AB<-A%*%B
rankMatrix(AB) # 2

## Checking stuff about matrix multiplication
A <- matrix(c(1,8,2,6), ncol=2)
B <- matrix(c(3,6,7,14), ncol=2)
AB <- A%*%B

A <- matrix(c(3,2,7,8), ncol=2)
B <- matrix(c(6,8,4,11), ncol=2)
# No matter if the first col of A and the first row of B are linearly
# dependent, AB is still ranl 2
A[,1] * 2 == B[1,]
det(A%*%B) # nonzero

# Strang Problem Set I.3, page 20
# 2: Find a square matrix with rank (A^2) < rank (A).
# This is basically asking a matrix A that has columns that are linearly independent
# when nonsquared, but that become linearly dependent when squared
A <- matrix(c(1, 1,
              -1, 1), ncol=2)
det(A)
det(A**2)

#Confirm that rank (ATA) =rank (A).
