t <- 3204859
theta <- 0.5
s <- vector(length=10)
for (i in 1:10)
  {
  x <- rbinom(t, 1, theta)
  s[i] <- sum(x)
}

s
cbind(s)
