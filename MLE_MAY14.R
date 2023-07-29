#Practice 1

y<- rnorm(5988,10.3,0.7)
T<- length(y)
like <- function(a)
  
{
  mu <- a[1]
  sigma <- a[2]
  lnl <- -0.5*T * log(2*pi*sigma^2)- (0.5/(sigma^2))*sum((y-mu)^2)
  lnl
}
mu.hat <- mean(y)
sigma.hat <- sd(y)
T<- length (y)

LM<- mu.hat- 1.96* sigma.hat/ sqrt(T)
UM <- mu.hat + 1.96* sigma.hat/sqrt(T)

LM
UM

like (c(5,0.7))
optiom (c(1,1), like)


#Practice 2
y<- rnorm(5988,10.3,0.7)
T<- length(y)
like <- function(mu, sigma)
  
{
 # mu <- a[1]
  #sigma <- a[2]
  lnl <- -0.5*T * log(2*pi*sigma^2)- (0.5/(sigma^2))*sum((y-mu)^2)
  lnl
}

like (10.3,0.7)
L <- vector(length=31)
m <- seq(9,12,0.1)
for(i in 1:31)
{
  L[i] <- like(mu=m[i],sigma=0.7)
}
cbind(m,L)



#practice 3
L<- matrix(nrow=31, col=10)
m<- seq(9,12,0.1); s<- seq(0.1,1,0.1)
for(i in 1:31)
{ for(j in 1:10)
{ L[i.j] <-like (mu=m[i], sigma = s[j])}
  
}


L