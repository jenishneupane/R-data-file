#mle, likelihood evaluationm. 
T <- 5988
mu <- log(45000)
sd <- 0.7
x <- rnorm(T, mu, sd )
hist(x, breaks= 50)
y <- exp(x)
hist(y, breaks=50)


#script 6
  y<- rnorm(5988,10.3,0.7)
T<- length(y)
like <- function(mu=mu, sigma=sigma)
  
{
  #mu <- a[1]
  #sigma <- a[2]
  lnl <- -0.5*T * log(2*pi*sigma^2)- (0.5/(sigma^2))*sum((y-mu)^2)
  -lnl
}

like(c(10, 0.7))
optim(c(1,1), like, hessian = TRUE)

mu.hat <- mean(y)
sigma.hat <- sd(y)
T <- length(y)
LM <- mu.hat - 1.96*sigma.hat/sqrt(T)
UM <- mu.hat + 1.96*sigma.hat/sqrt(T)


like(mu=seq(9, 12, 0.1), sigma=0.7)
L <- vector(length=3)
m <- seq(9, 12, 0.1)
for (i in 1:31) {
  L[i] <- like(mu=m[i], sigma=0.7)
}
cbind(m, L)
#like(mu = seq(9, 12, 0.1), sigma = 0.7)

l <- matrix(nrow = 31, ncol = 10)
s <- seq(0.1, 1, 0.1); m <- seq(9, 12, 0.1)
for (i in 1:31) {
  for (j in 1:10) {
    l[i,j] <- like (mu=m[i], sigma = s[j])
  }
}
View(l)

library(foreign)
getwd()
setwd("C:\\Users\\DELL\\OneDrive\\Documents")
data <- read.spss("poverty.sav")
View(data.frame(data))
attach(data)
y <- totcons_pc_7/ hhsize
hist(log(y), breaks = 50)
mean(log(y))
plot(log(y), type = "l")
summary(y)
x <- log(y)
summary(x)
sd(x)
z <- rnorm(10000, mean = 10, sd = 1)
mu.hat <- vector(length = 9988)
for (i in 11:10000) {
  mu.hat[i] <- mean(z[1:i+1]) 
  
}
plot(mu.hat[50:length(mu.hat)], type="l")
abline(h=10)

result <-  aggregate(totcons_pc_7~hhsize+ urbrur, FUN=mean)
result2 <- aggregate(totcons_pc_7~hhsize+ urbrur, FUN =var)
result
result2
model1 <- lm(log(totcons_pc_7)~ hhsize)
summary(model1)


model.whole <- lm(log(totcons_pc_7)~hhsize,data=data)
model.rur <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(urbrur=="rural"))
model.urb <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(urbrur=="urban"))
model.farwestern <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(region=="far-western"))
model.teraibelt <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(urbrur=="urban" & belt=="terai"))

                                                                
summary(model.whole)
summary(model.rur)
summary(model.urb)
summary(model.farwestern)
summary(model.teraibelt)

plot(data$hhsize,log(data$totcons_pc_7))
abline(model.whole$coef,col='yellow')
abline(model.rur$coef,col='green')
abline(model.urb$coef,col='blue')
legend("topright",c("whole","rural","urban"),lty=c(1,1,1),col=c("yellow","green","blue")) 


#for belt

model.mount <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="mountain"))
model.hill <- lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="hill"))
model.teraiii <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="terai"))                                              

model.urbmount <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="mountain" & urbrur=="urban"))

model.urbhill <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="hill" & urbrur=="urban"))

model.urbterai <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="terai" & urbrur=="urban"))


model.rurmount <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="mountain" & urbrur=="rural"))
model.rurhill <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="hill" & urbrur=="rural"))
model.rurterai <-lm(log(totcons_pc_7)~hhsize,data=data, subset=c(belt=="terai" & urbrur=="rural"))

model.rur
model.urb
model.mount                                                            
model.hill                                                                
model.teraiii
model.urbmount
model.urbhill
model.urbterai
model.rurmount
model.rurhill
model.rurterai

sum.urbmount <-summary(model.urbmount)
sum.urbhill<-summary(model.urbhill)
sum.urbterai<-summary(model.urbterai)
sum.rurmount<-summary(model.rurmount)
sum.rurhill<-summary(model.rurhill)
sum.rurterai<-summary(model.rurterai)

row.urbmount <-c(model.urbmount$coefficients,sum.urbmount$r.squared, sum.urbmount$df[2])
row.urbhill <-c(model.urbhill$coefficients,sum.urbhill$r.squared, sum.urbhill$df[2])
row.urbterai <-c(model.urbterai$coefficients,sum.urbterai$r.squared, sum.urbterai$df[2])
row.rurmount <-c(model.rurmount$coefficients,sum.rurmount$r.squared, sum.rurmount$df[2])
row.rurhill <- c(model.rurhill$coefficients,sum.rurhill$r.squared, sum.rurhill$df[2])
row.rurterai<- c(model.rurterai$coefficients,sum.rurterai$r.squared, sum.rurterai$df[2])

result <- cbind.data.frame("urban_mountain"=row.urbmount, "urban_hill"= row.urbhill, "urban_terai"=row.urbterai,
                           "rural_mountain"=row.rurmount, "rural_hill"=row.rurhill, "rural_terai"=row.rurterai)
rownames(result) <- c("Intercept", "Slope", "R Square", "Sample Size")
View(result)

#row.urb <-c(model.urb$coef,sum.urb$r.squared,sum.urb$df[2])

summary(model.mount)


