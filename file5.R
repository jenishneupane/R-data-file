
#finding the transpose and multiplying both vectors
T <- 100
X<- cbind(rep(1,T))
t(X) %*% X
X
t(X)
#solve(X,t(X))   this is wrong
#?solve


T <- 100
Y<- rnorm(T,mean = 10)
X<- cbind(rep(1,T))
X.X <-t(X) %*% X 
solve (X.X)
uc <- (t(X)%*%X)^(-1) %*% (t(X)%*%Y)
uc

sum(Y)/T

#for square
sum(Y^2)/T

ucsq <-


#sigma rule
xprimex<- t(X)%*%X; xprimex
yprimey<-t(Y)%*%Y; yprimey
xprimey<-t(X)%*%X; xprimey
yprimex<-t(Y)%*%X; yprimex

sigma.sq <- xprimex%*% (yprimey- yprimex%*% solve(xprimex)%*%xprimey)
sigma.sq



T <- 100
Y <- rnorm(T, mean = 10)
X <- cbind(rep(1, T))
X.X <- t(X) %*% X
solve(X.X)
uc <- solve(X.X) %*% (t(X) %*% Y)
uc

sum(Y) / T

xprimex <- t(X) %*% X
yprimey <- t(Y) %*% Y
xprimey <- t(X) %*% Y
yprimex <- t(Y) %*% X


x<- cbind(1,1:T)
beta.hat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
beta.hat
lm(Y~X)
