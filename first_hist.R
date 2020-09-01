#sigmaTrue=9,timepoint=100%
set.seed(456)
Nsim <-  4329
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 9 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 0 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n/2)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
}
rejectnullhat <-sum((pvalues<0.05))/4329
error <- sqrt(1/4329*(rejectnullhat - rejectnullhat^2))
hist(sizes,col="pink",main = "sample size when σT rue = 9")


#sigmaTrue=6.2,timepoint=100%
set.seed(456)
Nsim <-  4329
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 6.2 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 0 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
}
rejectnullhat <-sum((pvalues<0.05))/4329
error <- sqrt(1/4329*(rejectnullhat - rejectnullhat^2))
hist(sizes,col="pink",main = "sample size when σT rue = 6.2")

#sigmaTrue=5.4,timepoint=100%
set.seed(123)
Nsim <-  4329
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 5.4 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 0 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
}
rejectnullhat <-sum((pvalues<0.05))/4329
error <- sqrt(1/4329*(rejectnullhat - rejectnullhat^2))
hist(sizes,col="pink",main = "sample size when σT rue = 5.4")

#sigmaTrue=6.2,timepoint=100%
set.seed(456)
Nsim <-  4329
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 6.2 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 0 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
}
rejectnullhat <-sum((pvalues<0.05))/4329
error <- sqrt(1/4329*(rejectnullhat - rejectnullhat^2))
hist(sizes,col="pink",main = "sample size when σT rue = 6.2")

#sigmaTrue=12,timepoint=100%
set.seed(456)
Nsim <-  4329
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 12 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 0 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
}
rejectnullhat <-sum((pvalues<0.05))/4329
error <- sqrt(1/4329*(rejectnullhat - rejectnullhat^2))
hist(sizes,col="pink",main = "sample size when σT rue = 12")
