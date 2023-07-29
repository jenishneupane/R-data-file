T <- 5988
mu <- log(45000)
sd <- 0.7
x <- rnorm(T, mu, sd)
hist(x, breaks=50 )

y<- exp(x)
hist(y, breaks=  50)