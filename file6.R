#mean variance skewness kurtosis
library(moments)
library(readxl)
install.packages("writexl")
library(writexl)
# normal, uniform, student t
T <- 100
yt <- rnorm(T)
meant.norm <-mean (yt) ;meant.norm
vart.norm<- var (yt);vart.norm
skewt.norm<-skewness(yt);skewt.norm
kurt.norm<- kurtosis (yt);kurt.norm

ut <- runif (T)
meant.unif<- mean (ut);meant.unif
vart.unif<-var (ut);vart.unif
skewt.unif<- skewness(ut);skewt.unif
kurt.unif<- kurtosis (ut);kurt.unif

yst <- rt(T, df=5, ncp=1)
meant.rt<- mean(yst); meant.rt
vart.rt <- var(yst); vart.rt
skewt.rt <- skewness(yst); skewt.rt
kurt.rt <- kurtosis(yst); kurt.rt

?chisq.test
chi <- rchisq(T, df=5)
meant.rc <- mean(chi);meant.rc 
vart.rc<- var(chi); vart.rc
skewt.rc<- skewness(chi); skewt.rc
kurt.rc<- kurtosis(chi); kurt.rc


row.norm <- c(meant.norm,vart.norm,skewt.norm, kurt.norm)
row.unif <- c(meant.unif, vart.unif, skewt.unif, kurt.unif)
row.rt<- c(meant.rt, vart.rt,skewt.rt, kurt.rt)
row.chi <- c(meant.rc, vart.rc, skewt.rc, kurt.rc)
result <- cbind.data.frame("normality"= row.norm, "uniform"= row.unif, "student t"= row.rt, "chi sqaure"=row.chi)
rownames(result)<- c("Mean", "Variance", "Skewness", "Kurtosis")
trans.result <- t(result)
View (trans.result)


H<-1000
yth <- rnorm(H)
meanh.norm <-mean (yth) ;meanh.norm
varh.norm<- var (yth);varh.norm
skewh.norm<-skewness(yth);skewh.norm
kurh.norm<- kurtosis (yth);kurh.norm

uth <- runif (H)
meanh.unif<- mean (uth);meanh.unif
varh.unif<-var (uth);varh.unif
skewh.unif<- skewness(uth);skewh.unif
kurh.unif<- kurtosis (uth);kurh.unif

ysth <- rt(H, df=5, ncp=1)
meanh.rt<- mean(ysth); meanh.rt
varh.rt <- var(ysth); varh.rt
skewh.rt <- skewness(ysth); skewh.rt
kurh.rt <- kurtosis(ysth); kurh.rt

chi <- rchisq(H, df=5)
meanh.rc <- mean(chi);meanh.rc 
varh.rc<- var(chi); varh.rc
skewh.rc<- skewness(chi); skewh.rc
kurh.rc<- kurtosis(chi); kurh.rc

rowh.norm <- c(meanh.norm,varh.norm,skewh.norm, kurh.norm)
rowh.unif <- c(meanh.unif, varh.unif, skewh.unif, kurh.unif)
rowh.rt<- c(meanh.rt, varh.rt,skewh.rt, kurh.rt)
rowh.chi <- c(meanh.rc, varh.rc, skewh.rc, kurh.rc)

results <- cbind.data.frame("normality"= rowh.norm, "uniform"= rowh.unif, "student t"= rowh.rt, "chi square"=rowh.chi)
rownames(results)<- c("Mean", "Variance", "Skewness", "Kurtosis")
trans.results <- t(results)
View (trans.results)

totalresult <-cbind.data.frame( trans.result, trans.results)
rownames(totalresult) <- c("normality", "uniform", "student_t")
View(totalresult)

write_xlsx(totalresult, "F:\\Bachelor in Economics resources\\Semester 4\\econometrics I\\R data file\\distribution_difference_chiaddition.xlsx")

#chisquare test





