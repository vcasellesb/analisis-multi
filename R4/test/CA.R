# Studying from Correspondance analysis, by Greenacre
# https://www.routledge.com/Correspondence-Analysis-in-Practice/Greenacre/p/book/9780367782511

country <- c("Norway", "Canada", "Greece", "France/Germany")
typaday <- c("Holidays", "Half Days", "Full Days")
daytype_country <- matrix(c(6,1,4,2,
                            1,3,25,2,
                            11,11,0,20),
                          ncol = 3, nrow = 4)
rownames(daytype_country) <- country
colnames(daytype_country) <- typaday

daytype_country<- rbind(daytype_country, colSums(daytype_country))
daytype_country <- cbind(daytype_country, rowSums(daytype_country))

daytype_country / daytype_country[5,4] * 100

# Key point, gotten from https://stackoverflow.com/questions \ 
# /20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
# Let's say we have an n x n matrix, and an n vector:
n <- 3 
m <- matrix(rnorm(n*n), nrow=n, ncol=n)
v <- rnorm(n)

# Let's say I want to divide each row of m by the elements of v -- i.e. 
# the first row of m by the first component of v, etc
# Let's just try to do it using the division operator:
m[1,] / v[1]
m[2,] / v[2]
m[3,] / v[3]
m / v # is this what we want? YES

# What if we wanted to do it the other way?
# what if we wanted to divide each column of m by each of the elements of v?
m[,1] / v[1]
m[,2] / v[2]
m[,3] / v[3]
m / v

# As we can see, this doesn't work. This is key in correspondance analysis, 
# when we are trying to calculate column/row profiles. If the number of
# rows and columns of the matrix are equal, it will treat matrices as column vectors,
# and multiply "column-wise"


# Using this knowledge, let's say I wanna obtain the row profiles of daytype_country
# Row profiles means getting the sums of the rows of the data matrix X (so a n-dimensional vector,
# n being the number of rows of X). Therefore, we want to divide the columns of 
# X by the n-dimensional vector (or, equivalently, the rows of X by EACH of the elements
# of the n-dimensional vector -- i.e. X[1,] / ndim[1], ...)
# PROOF
X <- daytype_country; ndimensional <- rowSums(daytype_country)
all(X / ndimensional == rbind(X[1,]/ndimensional[1], X[2,]/ndimensional[2], 
                                          X[3,] / ndimensional[3], X[4,] / ndimensional[4])) # TRUE

# So, to get the row profiles:
daytype_country / rowSums(daytype_country) # we got it!

rowprofile <- function(mat, average=FALSE){
  res <- mat / rowSums(mat)
  if (average){
    average_row <- colSums(mat) / sum(mat)
    oldrownames <- rownames(res)
    res <- rbind(res, average_row)
    rownames(res) <- c(oldrownames, 'Average')}
  return(res)
}

round(rowprofile(daytype_country, TRUE),2)

# Now let's say we wanted to get the column profiles -- i.e. each column by its
# column total. This is a little bit trickier. This means that we want to divide 
# the columns of X by the sums of the columns of X, column-wise
# We want this:
ndimensional <- colSums(X)
cbind(X[,1] / ndimensional[1], X[,2] / ndimensional[2],
      X[,3] / ndimensional[3])

# Let's try it the easy way
all(X / colSums(X) == cbind(X[,1] / ndimensional[1], X[,2] / ndimensional[2],
                            X[,3] / ndimensional[3])) # FALSE

# What X / colSums(X) is actually doing is this:
all(X[1:3, 1] / colSums(X) == (X / colSums(X))[1:3,1])
all(c(X[4,1]/colSums(X)[1], X[1,2] / colSums(X)[2], X[2,2] / colSums(X)[3]) == c((X/colSums(X))[4,1],
                                                                        (X/colSums(X))[1,2],
                                                                        (X/colSums(X))[2,2]))
all(cbind(X[,1] / ndimensional[1], X[,2] / ndimensional[2],
      X[,3] / ndimensional[3]) == t(t(daytype_country) / colSums(daytype_country)))
# Sorry for this clusterfuck

# So, the way to calculate column profiles is:
column_profiles <- function(mat, average=FALSE){
  res <-t(t(mat) / colSums(mat))
  if (average) {
    average_col <- rowSums(mat) / sum(mat)
    oldcolnames <- colnames(res)
    res <- cbind(res, average_col)
    colnames(res) <- c(oldcolnames, 'Average')}
  res
}

# Bayes stuff
X <- daytype_country
P_norway_and_holidays = X[1,1] / sum(X)
P_norway <- rowSums(X)[1] / sum(X)
P_holidays <- colSums(X)[1] / sum(X)

P_norway_given_holiday <- P_norway_and_holidays / P_holidays
P_holidays_given_norway <- P_norway_and_holidays / P_norway

P_norway_given_holiday / P_norway
P_holidays_given_norway / P_holidays

### Chapter 3
education <- matrix(c(5,18,19,12,3,
                      7,46,29,40,7,
                      2,20,39,49,16),ncol=3)
rownames(education) <- c('Some primary', 'Primary completed',
                         'Some secondary', 'Secondary completed',
                         'Some tertiary')
colnames(education) <- c("Glance", "Fairly thorough", "Very thorough")

# It took me so long to understand the following sentence:
# Average profile is also a weighted average of the profiles themselves

# PROOF
average <- column_profiles(education, T)[,4]
max(abs(colSums(rowprofile(education) * average) - rowprofile(education, T)[6,])) # e-17

# Same with columns
average <- rowprofile(education, T)[6,]
max(abs(colSums(t(column_profiles(education)) * average)) - column_profiles(education, T)[,4]) # 0

######################################## 
# Section <<Merging rows or columns>>

E1_and_2 <- education[1,] + education[2,]
new_education <- rbind(E1_and_2, education[3:5,])
r <- rowprofile(education)
r1 <- r[1,]; r2 <- r[2,]

# Row 1 and 2 masses:
m1 <- column_profiles(education, T)[1, 4]
m2 <- column_profiles(education, T)[2,4]

# Two ways of obtaining the same
v1 <- r1 * m1/(rowSums(new_education)[1] / sum(education)) + 
  r2 * m2/(rowSums(new_education)[1] / sum(education))
v2 <- rowprofile(new_education)[1,]
max(abs(v1 - v2)) # e-17

#####################
new_table <- rowprofile(education)
column_profiles(new_table, T)

######################################## 
# Chapter 4
# What is a chi-squared test for contingency tables?
# Basically, you assume that one of the two variables you have
# has no effect on the frequencies that you have observed
# For example, let's say we wanna test homogeneity between the factors of 
# the variable encoded in the rows of the table
# We would aggregate the info encoded in the rows (thus, computing the rowSums)
# of the table and we would use those frequencies (rowSums/totalSum) to compute
# what we would expect if all rows were the same (there was no differences between rows)
# Then, we would compare this "fake" expected table to the table that we actually have,
# otherwise known as the "Observed" table that contains the observed frequencies

# This is what the next function does
expected_freq <- function(mat, rowwise=T){
  n <- nrow(mat); m <- ncol(mat)
  it <- if (rowwise) n else m
  new_mat <- matrix(0, ncol = m, nrow=n)
  for (i in 1:it){
    if (rowwise) new_mat[i, ] <- rep(rowSums(mat)[i], m)
    else new_mat[, i] <- rep(colSums(mat)[i], n)
  }
  
  if (rowwise)
    new_mat <- t(t(new_mat) * colSums(mat) / sum(mat))
  else new_mat <- new_mat * rowSums(mat) / sum(mat)
  dimnames(new_mat) <- dimnames(mat)
  new_mat
}

E <- expected_freq(education, F)
E

# Function to calculate Expected and Observed frequencies in a contingency table
chisq_test <- function(mat){
  E <- expected_freq(mat)
  E <- as.vector(E)
  O <- as.vector(mat)
  subs <- O - E
  subs <- subs ** 2
  subs <- subs / E
  sum(subs)
}

Xi <- chisq_test(education)
df <- prod(dim(education) - 1)
pval <- pchisq(q=Xi, df=df, lower.tail = F)


inertia <- function(mat){
  row_masses <- column_profiles(mat,T)[, (ncol(mat)+1)]
  O <- rowprofile(mat)
  E <- rowprofile(expected_freq(mat))
  res <- (O-E)**2
  res <- res / E
  res <- res * row_masses 
  
  # Check inertia and chisq statistic / sum of table 
  # are equal
  stopifnot(sum(res) - chisq_test(mat) / sum(mat) < 1e-15)
  
  sum(res)
}
inertia(education)
