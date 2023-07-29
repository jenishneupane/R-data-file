n<- 1000
b0 <- 2
b1 <- 1.5
sigma <- 1

x<- rnorm(n, mean=1, sd= sigma)
e <- rnorm(n, mean=0, sd=sigma*x^2)
y<- b0+b1*x+e
model <- lm(y~x)
plot(x,y)
model
summary(model)


#White test
u_hat <- model$residuals; u_hat

uhat_sq <- u_hat^2
plot(x, uhat_sq)

aux <- lm(uhat_sq~ x+ I(x^2)+ I(x^3))
summary_aux<- summary(aux)[8]$r.squared

Lm_test <- n*summary_aux
Lm_test
pchisq(Lm_test,2,lower.tail = FALSE)








