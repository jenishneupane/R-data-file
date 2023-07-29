
#model 1
library(foreign)
getwd()
data <- data.frame(read.spss("poverty.sav", to.data.frame = TRUE))

pcc <- data$totcons_pc_7
hhsize <- data$hhsize
model.1 <- lm (pcc~hhsize)
u.hat <- model.1$residuals
hist(u.hat)


library(moments)
sk <-skewness(u.hat); k<- kurtosis(u.hat); sk
JB<- length(pcc)* ((sk^2)/6+ ((k-3)^2/24)); JB

pvalue <- pchisq(JB, 2, lower.tail = FALSE); pvalue


#model 2

model.2 <- lm(log(pcc)~hhsize+ I(hhsize^2)+ I(hhsize^3), subset= c(data$urbru=="urban"))
summary(model.2)
u.hat <- model.2$residuals
hist(u.hat)
sk <-skewness(u.hat); k<- kurtosis(u.hat); sk
JB<- length(pcc)* ((sk^2)/6+ ((k-3)^2/24)); JB

pvalue <- pchisq(JB, 2, lower.tail = FALSE); pvalue 








