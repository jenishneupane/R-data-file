sample <- 200
mu <- 5 ; sigma2 <- 10

u.t <- rnorm(sample, 0, sqrt(sigma2))
plot (u.t)
hist (u.t)

x.t <- mu + u.t

plot (x.t,type='b')


?read.csv

install.packages("foreign")
library(foreign)

?read.dta



