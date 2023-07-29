T <- 1000
Xt <- rnorm(T, mean = 2, sd = 3)
ut <- rnorm(T, mean = 0, sd = 1)
?rnorm
Y.t <- 5+2*Xt+0.2*Xt^2+0.2*Xt^2+ut
model.3 <- lm(Y.t~Xt) #mis_specified_regression
summary(model.3)

y.hat <- model.3$fitted.values
?I
model.4 <- lm(Y.t ~ y.hat + I(y.hat^2) + I(y.hat^3)) #auxiliary_regression
summary(model.4)
?lm
#Correct_model
model.5 <- lm(Y.t~Xt +I(Xt^2))
summary(model.5)
y.hat1 <- model.5$fitted.values
model.6 <- lm(Y.t~y.hat1 + I(y.hat1^2)+ I(y.hat1^3))
summary(model.6)