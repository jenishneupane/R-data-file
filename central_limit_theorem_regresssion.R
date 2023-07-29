n <- 100 

beta0 <- 2
beta1 <- 3
sigma <- 5


iterations <- 1000
vector<- numeric(iterations)

for (i in 1:iterations) {
  e <- rnorm(n, mean = 0, sd = sigma)
  
  X <- rnorm(n, mean = 10, sd = 2)
  Y <- beta0 + beta1 * X + e
  
  model <- lm(Y ~ X)
  model
  
  beta_hat <- coef(model)[2]  
  beta_hat
}

mean_beta_hat <- mean(beta_hat); mean_beta_hat
sd_beta_hat <- sd(beta_hat); sd_beta_hat






  