## Exercici 1

cols <- c("A", "B", "C")
rows <- paste0("P", 1:6, sep='')
dt <- matrix(c(0.130, 0.11, 0.09,
               0.105, 0.065, 0.08,
               0.07, 0.035, 0.045,
               0.08, 0.04, 0.08,
               0.01, 0.02, 0.02,
               0.005, 0.01, 0.005), ncol=3, byrow=T)
colnames(dt) <- cols
rownames(dt) <- rows
rowSums(dt) # marginal distributions of rows
colSums(dt) # marginal distributions of cols

# conditional probabilities given columns
t(t(dt) / colSums(dt))
