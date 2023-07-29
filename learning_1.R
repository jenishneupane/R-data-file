#this file will be about the things that i will learn in R programming
#course provided by Coursera

getwd()
dir()

x<- rnorm(100)
my_fun<- x+ rnorm(length(x))
my_fun

vector("numeric", length = 5)
1/0
1/Inf
#Inf represents infinity
0/0
# NaN is an undefined number. It can also be thought as missing value

y<- c(TRUE, 2)
y
# TRUE acts as 1 and FALSE acts as 0
# when different types of objects are binded in same variable,
# coersion occurs so that every objects can be treated as same

z<- 0:6
class(z)  
as.numeric(z)
as.logical(z)

#matrix

m <- matrix(1:6 ,nrow = 2, ncol = 3)
m

dim(m)
attributes(m)
#here, the values are first filled in column














