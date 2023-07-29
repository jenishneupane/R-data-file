T <- 1000
b0 <- 2
b1 <- 1.5
b2 <- 0.9

Xt <- rnorm(T, mean= 0, sd= sigma)
ut <- rnorm(T, mean= 0, sd= sigma)

Yt <- vector(length = T)
Yt[1] <-0
for (t in 2: T){
  Yt[t] <- b0 + (b1*Xt[t]) + (b2*Yt[t-1]) + ut[t]
}

Yt
plot(Yt)
