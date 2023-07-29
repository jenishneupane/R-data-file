#the code generates multple random samples of size T
theta <-0.5
T <-3204859
Ysum <-1635176
#creating a S vector with lenghth 10
S <- vector(length=200)
for (i in 1:10) 
{
  s <-rbinom(T, 1, theta) #Generates a random sample of size T 
  S[i] <- sum(s)
  print(s)  
}
mean (s) #calculate the mean