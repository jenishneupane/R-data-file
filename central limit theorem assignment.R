n <- 1000
num_iterations <- 10000  
b0 <- 2
b1 <- 3
sigma <- 5


beta_hat_vec <- numeric(num_iterations)


for (i in 1:num_iterations) {

  X <- rnorm(n, mean = 5, sd = 2)
  
  e<- rnorm(n, mean = 0, sd = sigma)
  Y <- b0 + b1 * X + e
  
  
  model <- lm(Y ~ X)
  
 
  beta_hat[i] <- coef(model)[2]
}

library(moments)
kurt <- kurtosis(beta_hat); kurt
skew <- skewness(beta_hat); skew

