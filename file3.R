#calculates the log-likelihood values for different values of 'theta'
Ysum <- 1635176
T <- 3204859
theta <- seq(0.01, 0.90, 0.01)
l <- T*log(1-theta)+log(theta)*Ysum-log(1-theta)*Ysum;cbind(theta,1)
cbind(theta, 1) [l=max(l)]
plot(theta, l, type = 'l', col='orange', lwd=4)
abline(v=0.51, col='yellow', lwd=3)
abline(h=max(l),col='black', lwd=4)
#The 'abline' function is used to add additional lines to the plot



#the below code includes following thingS:
#Normal distribution, time series, t distribution,chi square, uniform
#distribution
T <- 100
x <- rnorm(T)
plot(x,type = "l")
hist(x)
library(moments)
skewness(x); kurtosis(x)
t <- sin(1:T) + cos(1:T) + 0.1*c(1:T)
d <- x + t
plot(d, type = "l")
h <- vector(length = T)
h[1] <- 0 ; h[2] <- 0.1 
for (t in 3:T) {
  h[t] <- 0.85*h[t-1] - 0.01*d[t-2] + x[t]  
}
plot(h, type="l")

y <- rt(T, df=1)
plot(y,type = "l")
hist(y, breaks = 50)
skewness(y); kurtosis(y)
t <- sin(1:T) + cos(1:T) + 0.1*c(1:T)
e <- y + t
plot(e, type = "l")

z <- runif(T)
plot(z,type = "l")
hist(z, breaks = 50)
skewness(z); kurtosis(z)
t <- sin(1:T) + cos(1:T) + 0.1*c(1:T)
f <- z + t
plot(f, type = "l")

w <- rchisq(T,df=1)
plot(w,type = "l")
hist(w, breaks = 50)
skewness(w); kurtosis(w)
t <- sin(1:T) + cos(1:T) + 0.1*c(1:T)
g <- w + t
plot(g, type = "l")
