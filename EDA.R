T <- 100
x <- rnorm("10")
plot(x, type= "1")
hist(x)
library(moments)
skewness(x); kurtosis(x)
t <- sin(1:T) + cos(1:T) + 0.1* c(1:T)
d <- x+ 0.5*t
plot (d, type= "l")



d <- vector(length = T)
for (t in 2:T) {
  d(t) <- 0.9*d[t-1]+ x[t]
}
plot (d, type="l")









y<- rt(T, df=1)
plot (y, type="1")
hist (y, breaks= 50)
skewness(y); kurtosis(y)
t <- sin(1:T) + cos(1:T) + 0.1* c(1:T)
e <- y+ 0.5*t
plot (d, type= "l")

z <- runif(10)
plot (z, type= "1")
hist(z, breaks= 50)
skewness(z); kurtosis(z)
t <- sin(1:T)+ cos(1:T)+ 0.1*c(1:T)
g <- z+t
plot(g, type="l")


# rchisq ko garna baaki

