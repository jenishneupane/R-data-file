T <- 1000
B_0 <- 2
B_1 <- 1.5
B_2 <- 0.9
Xt <- rnorm(T, mean = 0, sd = 1)
ut <- rnorm(T, mean = 0, sd = 1)
Yt <- vector(length = T)
Yt[1] <- 0
for (t in 2:T) {
  Yt[t] <- 2 + 1
  .5*Xt[t] -0.9*Yt[t-1] + ut[t]
}

plot(Yt, type = 'l')
model <- lm(Yt~Xt)
summary(model) 


u.haat <- model$residuals


auxmodel <- lm(u.haat[2:T]~u.haat[1:(T-1)]-1)

summary(auxmodel)


rho_hat <- auxmodel$coefficients
d <- 2*(1-rho_hat) 

d

