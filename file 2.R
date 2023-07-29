theta <- 0.5
N <- 10000
T <- 20
X <- matrix(nrow = T, ncol = N)

for (j in 1:N) {
  X[ , j] <- rbinom(T, 1, theta)
}
View(X) # Opens a data viewer to display the matrix 'X'
mean(X)
dim(X)# Returns the dimensions of the matrix 'X'

theta.hat <- colMeans(X)   # Calculates the column-wise means of matrix 'X
hist(theta.hat, breaks = 20) # Plots a histogram of the values in 'theta.hat' with 20 breaks
mean(theta.hat)
# Estimation using different subsets of data

theta.hat1 <- X[1, ]
theta.hat2 <- X[1, ]+X[2, ]
theta.hat3 <- 1/2*(X[1,]+X[T,])
theta.hat4 <- X[1,]-X[2,]
theta.hat5 <- X[1,]+X[2,]+X[3,]
M <- c(mean(theta.hat), mean(theta.hat1), mean(theta.hat2), mean(theta.hat3), mean(theta.hat4), mean(theta.hat5))
V <- c(var(theta.hat), var(theta.hat1), var(theta.hat2), var(theta.hat3), var(theta.hat4), var(theta.hat5))
cbind(M, V)
A <- cbind("Mean"= M, "Variance"= V)
rownames(A) <- c("theta.hat", "theta.hat1", "theta.hat2", "theta.hat3", "theta.hat4", "theta.hat5")
par(mfcol=c(2,3))# Sets the plotting layout to have 2 rows and 3 columns for the following histograms
hist(theta.hat, breaks = 30)
hist(theta.hat1, breaks = 30)
hist(theta.hat2, breaks = 30)
hist(theta.hat3, breaks = 30)
hist(theta.hat4, breaks = 30)
hist(theta.hat5, breaks = 30)
