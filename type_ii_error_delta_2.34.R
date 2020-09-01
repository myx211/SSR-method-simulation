#Different scenario Type II error
#Ceiling=300 per group
#Bottom = 30 per group


#initial
set.seed(456)
sigma <- 6.2
Delta <- 2.34
alpha <- 0.05
beta <- 0.2
delta <- 2.34  #h0 is rejected
muC <- 11.7
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
betahat <- mean(rejectnull)
error <- sqrt(1/17312*(alphahat - alphahat^2))
n
betahat
error
hist(sizes)

#sigmaTrue=9,timepoint=25%
set.seed(456)
Nsim <-  19312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 9 #the true sigma
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
rejectnull25 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull25 - rejectnull25^2))
rejectnull25
error
n1
hist(sizes)

#sigmaTrue=9,timepoint=50%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 9 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 2.34 #hypothesis H0 is rejected
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
rejectnull50 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull50 - rejectnull50^2))
error
rejectnull50
n1
hist(sizes)

#sigmaTrue=9,timepoint=75%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 9 #the true sigma
sigma<- 6.2  
Delta<- 2.34
alpha<- 0.05 
beta<- 0.2
muC<- 11.7
delta<- 2.34 #hypothesis H0 is rejected
muT<- muC - delta
n <- round(2*(sigma^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
n0 <- round(3*n/4)
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
rejectnull75 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull75 - rejectnull75^2))
n1
error
rejectnull75

#sigmaTrue=5.4,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 5.4 #the true sigma
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
rejectnull25 <-sum((pvalues<0.05))/17312
error <- sqrt(1/17312*(rejectnull25 - rejectnull25^2))
rejectnull25
error
n1
hist(sizes)

#sigmaTrue=5.4,timepoint=50%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 5.4 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=5.4,timepoint=75%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 5.4 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=6.2,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 6.2 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=6.2,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 6.2 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=6.2,timepoint=75%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 6.2 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=12,timepoint=25%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 12 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=12,timepoint=50%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 12 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1

#sigmaTrue=12,timepoint=50%
set.seed(456)
Nsim <-  17312
pvalues <- rep(NA, Nsim)
sizes <- rep(NA, Nsim)
sigmaTRUE <- 12 #the true sigma
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
  resultsC0 <- rnorm(n0, muC, sigmaTRUE) 
  resultsT0 <- rnorm(n0, muT, sigmaTRUE)
  interimdata <- c(resultsC0, resultsT0)
  sigma1 <- sd(interimdata)
  n1 <- round(2*(sigma1^2)*((qnorm(1 - alpha) + qnorm(1 - beta))^2)/(Delta^2))
  n1 <- max(n1, n)
  if(n1>300) n1<-300
  if(n1<30)  n1<-30#ceiling and bottom
  resultsC1 <- rnorm(n1 - n0, muC, sigmaTRUE) 
  resultsT1 <- rnorm(n1 - n0, muT, sigmaTRUE)
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
n1
