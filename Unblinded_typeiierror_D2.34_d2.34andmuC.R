#Different scenario Type II error (Unblinded SSR)
#Ceiling=300 per group
#Bottom = 30 per group

#initial
set.seed(456)
sigma <- 6.2
Delta <- 2.34
alpha <- 0.05
beta <- 0.2
muC <- 8.6
delta <- 0.2*muC
muT <- muC - delta
Nsim <-17312
pvalues <- rep(NA, Nsim)
for(i in 1:Nsim){
  n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  resultsC <- rnorm(n, muC, sigma) 
  resultsT <- rnorm(n, muT, sigma)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
}
rejectnull <- (pvalues < 0.05)
betahat<- mean(rejectnull)
error <- sqrt(1/17312*(alphahat - alphahat^2))
n
betahat
error
hist(sizes)

#Delta=2.34,delta=2.34,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 2.34 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n/4)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull25 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull25 - rejectnull25^2))
rejectnull25
error

#Delta=2.34,delta=2.34,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 2.34 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n/2)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull50 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull50 - rejectnull50^2))
rejectnull50
error

#Delta=2.34,delta=2.34,timepoint=75%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 2.34 #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(3*n/4)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull75 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull75 - rejectnull75^2))
rejectnull75
error


#Delta=2.34,delta=0.2*muC,timepoint=75%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 8.6
delta<- 0.2*muC #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(3*n/4)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull75 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull75 - rejectnull75^2))
rejectnull75
error


#Delta=2.34,delta=0.2*muC,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 8.6
delta<- 0.2*muC #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n/4)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull25 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull25 - rejectnull25^52))
rejectnull25
error

#Delta=2.34,delta=0.2*muC,timepoint=50%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 8.6
delta<- 0.2*muC #hypothesis H0
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(n/2)
for(i in 1:Nsim){
  resultsC0 <- rnorm(n0, muC, sigma) 
  resultsT0 <- rnorm(n0, muT, sigma)
  interimdata <- c(resultsC0,resultsT0)
  delta1 <- mean(resultsC0)-mean(resultsT0)
  n1 <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(delta1^2))
  n1 <- max(n1, n)
  if(n1>300) 
    n1<-300
  if(n1<30)  
    n1<-30
  resultsC1 <- rnorm(n1 - n0, muC, sigma) 
  resultsT1 <- rnorm(n1 - n0, muT, sigma)
  resultsC <- c(resultsC0, resultsC1) 
  resultsT <- c(resultsT0, resultsT1)
  outcome <- t.test(resultsC, resultsT, var.equal = TRUE, alternative = "greater")
  pvalues[i] <- outcome$p.value 
  sizes[i] <- n1
  
}
rejectnull50 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull50 - rejectnull50^2))
rejectnull50
error
