#######################
# STAT 927
# Final project
#######################

# read in data
mall = read.csv("/Users/haowei/Documents/UPenn documents/Bayesian statistics/final project/Mall_Customers.csv", colClasses=c("NULL", NA, NA,NA,NA))
colnames(mall) = c("gender", "age", "annual_income", "spending")

hist(mall$spending)

## First chain
# initial value
alpha = 0.5
y = mall$spending
X = mall[,1:3]
X["gender"] = ifelse(X$gender == 'Male', 1, 0)
ones = rep(1, n)
X = cbind(ones, X)
model1 <- lm(spending ~ gender+age+annual_income, data=mall[1:100,])
model0 <- lm(spending ~ gender+age+annual_income, data=mall[101:200,])
beta1 = model1$coefficients 
beta0 = model0$coefficients 
sigsq = summary(model1)$sigma^2
# sigsq2 = summary(model0)$sigma^2
V.beta1 = summary(model1)$cov.unscaled
V.beta0 = summary(model0)$cov.unscaled
n <- dim(mall)[1]
p <- length(beta1)

numiters <- 20100
# params <- matrix(NA,nrow=numiters,ncol=1)
alpha.samp <- rep(NA, numiters)
sigsq.samp <- rep(NA, numiters)
beta1.samp <- matrix(NA, nrow = numiters, ncol=p)
beta0.samp <- matrix(NA, nrow = numiters, ncol=p)
alpha.samp[1] <- alpha
sigsq.samp[1] <- sigsq
beta1.samp[1, ] <- beta1
beta0.samp[1,] <- beta0
inds <- matrix(NA,nrow=numiters-1,ncol=n)

library(mvtnorm)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA, n)
  for(j in 1:n){
    a <- alpha * exp(-(y[j]-sum(X[j,]*beta1))^2/(2*sigsq))
    b <- alpha * exp(-(y[j]-sum(X[j,]*beta0))^2/(2*sigsq))
    p <- a/(a+b)
    I[j] <- rbinom(1,1,p)
  }
  ## calculating statistics from indicator variables
  n1 <- sum(I==1) # high-spender
  n0 <- sum(I==0) # low-spender
  alpha <- rbeta(1, n1+1, n0+1)
  x1temp <- as.matrix(X[I==1, 2:4])
  x0temp <- as.matrix(X[I==0, 2:4])
  y1temp <- y[I==1]
  y0temp <- y[I==0]
  # use regression to find mean and variance of multivarate normal
  model1 <- lm(y1temp~x1temp)
  beta1mean <- model1$coefficients
  V.beta1 = summary(model1)$cov.unscaled
  model0 <- lm(y0temp~x0temp)
  beta0mean <- model0$coefficients
  V.beta0 = summary(model0)$cov.unscaled
  beta1 <- rmvnorm(1, mean = beta1mean, sigma=sigsq*V.beta1)
  beta0 <- rmvnorm(1, mean = beta0mean, sigma=sigsq*V.beta0)
  # compute sigma squared
  pred.y1 <- as.vector(as.matrix(X[I==1,]) %*% t(beta1))
  pred.y0 <- as.vector(as.matrix(X[I==0,]) %*% t(beta0))
  ss <- sum((y[I==0]-pred.y0)^2) + sum((y[I==1]-pred.y1)^2)
  temp <- rgamma(1, shape=(n-p)/2, rate=ss/2)
  sigsq <- 1/temp
  ## storing current values
  alpha.samp[i] <- alpha
  sigsq.samp[i] <- sigsq
  beta1.samp[i,] <- beta1
  beta0.samp[i,] <- beta0
  inds[i-1,] <- I
}

alpha.samp1 <- alpha.samp
sigsq.samp1 <- sigsq.samp
beta1.samp1 <- beta1.samp
beta0.samp1 <- beta0.samp
inds1 <- inds

## Second chain
# initial value
alpha = 0.8
y = mall$spending
X = mall[,1:3]
X["gender"] = ifelse(X$gender == 'Male', 1, 0)
ones = rep(1, n)
X = cbind(ones, X)
model1 <- lm(spending ~ gender+age+annual_income, data=mall[1:100,])
model0 <- lm(spending ~ gender+age+annual_income, data=mall[101:200,])
# beta1 = model1$coefficients 
# beta0 = model0$coefficients 
# assuming group 1 is high-spender
beta1 = c(70, 1, 1, 1)
beta0 = c(50, -1, -1, -1)
sigsq = 300
# sigsq2 = summary(model0)$sigma^2
V.beta1 = summary(model1)$cov.unscaled
V.beta0 = summary(model0)$cov.unscaled
n <- dim(mall)[1]
p <- length(beta1)

numiters <- 20100
# params <- matrix(NA,nrow=numiters,ncol=1)
alpha.samp <- rep(NA, numiters)
sigsq.samp <- rep(NA, numiters)
beta1.samp <- matrix(NA, nrow = numiters, ncol=p)
beta0.samp <- matrix(NA, nrow = numiters, ncol=p)
alpha.samp[1] <- alpha
sigsq.samp[1] <- sigsq
beta1.samp[1, ] <- beta1
beta0.samp[1,] <- beta0
inds <- matrix(NA,nrow=numiters-1,ncol=n)

library(mvtnorm)
for (i in 2:numiters){
  ## sampling indicator variables
  I <- rep(NA, n)
  for(j in 1:n){
    a <- alpha * exp(-(y[j]-sum(X[j,]*beta1))^2/(2*sigsq))
    b <- alpha * exp(-(y[j]-sum(X[j,]*beta0))^2/(2*sigsq))
    p <- a/(a+b)
    I[j] <- rbinom(1,1,p)
  }
  ## calculating statistics from indicator variables
  n1 <- sum(I==1) # high-spender
  n0 <- sum(I==0) # low-spender
  alpha <- rbeta(1, n1+1, n0+1)
  x1temp <- as.matrix(X[I==1, 2:4])
  x0temp <- as.matrix(X[I==0, 2:4])
  y1temp <- y[I==1]
  y0temp <- y[I==0]
  # use regression to find mean and variance of multivarate normal
  model1 <- lm(y1temp~x1temp)
  beta1mean <- model1$coefficients
  V.beta1 = summary(model1)$cov.unscaled
  model0 <- lm(y0temp~x0temp)
  beta0mean <- model0$coefficients
  V.beta0 = summary(model0)$cov.unscaled
  beta1 <- rmvnorm(1, mean = beta1mean, sigma=sigsq*V.beta1)
  beta0 <- rmvnorm(1, mean = beta0mean, sigma=sigsq*V.beta0)
  # compute sigma squared
  pred.y1 <- as.vector(as.matrix(X[I==1,]) %*% t(beta1))
  pred.y0 <- as.vector(as.matrix(X[I==0,]) %*% t(beta0))
  ss <- sum((y[I==0]-pred.y0)^2) + sum((y[I==1]-pred.y1)^2)
  temp <- rgamma(1, shape=(n-p)/2, rate=ss/2)
  sigsq <- 1/temp
  ## storing current values
  alpha.samp[i] <- alpha
  sigsq.samp[i] <- sigsq
  beta1.samp[i,] <- beta1
  beta0.samp[i,] <- beta0
  inds[i-1,] <- I
}

alpha.samp2 <- alpha.samp
sigsq.samp2 <- sigsq.samp
beta1.samp2 <- beta1.samp
beta0.samp2 <- beta0.samp
inds2 <- inds

#examining samples
par(mfrow=c(2,1))
plot(1:100,alpha.samp[1:100],type="l")
plot(1:100,sigsq.samp[1:100],type="l")
plot(1:100,beta1.samp[1:100,1], type="l")
plot(1:100,beta0.samp[1:100,1], type="l")

# check convergence
ymin<-min(alpha.samp1,alpha.samp2)
ymax<-max(alpha.samp1,alpha.samp2)
plot(1:200,alpha.samp1[1:200],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,alpha.samp2[1:200],col=3)

ymin<-min(beta1.samp1[,1],beta1.samp2[,1])
ymax<-max(beta1.samp1[,1],beta1.samp2[,1])
plot(1:200,beta1.samp1[1:200,1],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,beta1.samp2[1:200,1],col=3)

ymin<-min(beta1.samp1[,2],beta1.samp2[,1])
ymax<-max(beta1.samp1[,2],beta1.samp2[,1])
plot(1:200,beta1.samp1[1:200,2],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,beta1.samp2[1:200,2],col=3)

ymin<-min(beta0.samp1[,1],beta0.samp2[,1])
ymax<-max(beta0.samp1[,1],beta0.samp2[,1])
plot(1:200,beta0.samp1[1:200,1],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,beta0.samp2[1:200,1],col=3)

ymin<-min(beta0.samp1[,3],beta0.samp2[,1])
ymax<-max(beta0.samp1[,3],beta0.samp2[,1])
plot(1:200,beta0.samp1[1:200,3],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,beta0.samp2[1:200,3],col=3)

ymin<-min(beta0.samp1[,4],beta0.samp2[,1])
ymax<-max(beta0.samp1[,4],beta0.samp2[,1])
plot(1:200,beta0.samp1[1:200,4],type="l",col=2,ylim=c(ymin,ymax))
lines(1:200,beta0.samp2[1:200,4],col=3)

# keep a record of original data
alpha.samp1.old <- alpha.samp1
alpha.samp2.old <- alpha.samp2
sigsq.samp1.old <- sigsq.samp1
sigsq.samp2.old <- sigsq.samp2
beta0.samp1.old <- beta0.samp1
beta0.samp2.old <- beta0.samp2
beta1.samp1.old <- beta1.samp1
beta1.samp2.old <- beta1.samp2
inds1.old <- inds1
inds2.old <- inds2

# throw away burnout
burnout <- 101
alpha.samp1 <- alpha.samp1[burnout:numiters]
alpha.samp2 <- alpha.samp2[burnout:numiters]
sigsq.samp1 <- sigsq.samp1[burnout:numiters]
sigsq.samp2 <- sigsq.samp2[burnout:numiters]
beta0.samp1 <- beta0.samp1[burnout:numiters,]
beta0.samp2 <- beta0.samp2[burnout:numiters,]
beta1.samp1 <- beta1.samp1[burnout:numiters,]
beta1.samp2 <- beta1.samp2[burnout:numiters,]
inds1 <- inds1[100:20099,]
inds2 <- inds2[100:20099,]

# check for autocorrelation
par(mar=c(1,1,1,1))
par(mfrow=c(4,1))
acf(alpha.samp1,lag.max=50)
acf(alpha.samp2,lag.max=50)
acf(beta1.samp1[,1],lag.max=50)
acf(beta1.samp2[,1],lag.max=50)
acf(beta0.samp1[,1],lag.max=50)
acf(beta0.samp2[,1],lag.max=50)

numpostburn <- length(alpha.samp1)

# keep every 10 iterations
thin.param <- 10
temp <- thin.param*(1:(numpostburn/thin.param))

alpha.samp1.thin <- alpha.samp1[temp]
alpha.samp2.thin <- alpha.samp2[temp]
beta0.samp1.thin <- beta0.samp1[temp,]
beta0.samp2.thin <- beta0.samp2[temp,]
beta1.samp1.thin <- beta1.samp1[temp,]
beta1.samp2.thin <- beta1.samp2[temp,]
sigsq.samp1.thin <- sigsq.samp1[temp]
sigsq.samp2.thin <- sigsq.samp2[temp]
inds1.thin <- inds1[temp,]
inds2.thin <- inds2[temp,]

# check for autocorrelation after thinning
acf(alpha.samp1.thin,lag.max=20)
acf(alpha.samp2.thin,lag.max=20)
acf(beta0.samp1.thin[,1],lag.max=20)
acf(beta0.samp1.thin[,1],lag.max=20)
acf(beta0.samp2.thin[,1],lag.max=20)
acf(beta0.samp2.thin[,1],lag.max=20)
acf(beta1.samp1.thin[,2],lag.max=20)
acf(beta1.samp1.thin[,2],lag.max=20)

# combine samples from two chains
alpha.final <- c(alpha.samp1.thin, alpha.samp2.thin)
sigsq.final <- c(sigsq.samp1.thin, sigsq.samp2.thin)
beta0.final <- rbind(beta0.samp1.thin, beta0.samp2.thin)
beta1.final <- rbind(beta1.samp1.thin, beta1.samp2.thin)
inds.final <- rbind(inds1.thin, inds2.thin)

c(mean=mean(alpha.final), ci = quantile(alpha.final, c(0.025, 0.975)))
c(mean=mean(sigsq.final), ci = quantile(sigsq.final, c(0.025, 0.975)))
for(i in 1:4){
  print(i)
  print(c(mean=mean(beta0.final[,i]), ci = quantile(beta0.final[,i], c(0.025, 0.975))))
  print(c(mean=mean(beta1.final[,i]), ci = quantile(beta1.final[,i], c(0.025, 0.975))))
}

## visualize result
# histogram of spending with two groups
hist(mall$spending, main = "Customer Spending Score Distribution", xlab = "Spending Score", breaks = 20)
mean.inds = colMeans(inds.final)
# tag customer as high-spender if assigned to group 1 half of the time
group.idx = ifelse(mean.inds>=0.8, 1, 0) 
high.spending = mall$spending[group.idx==1]
low.spending = mall$spending[group.idx==0]
plot(density(low.spending), col="cornflowerblue", main = "Segmented Spending Score Density", xlab="Spending Score")
lines(density(high.spending), col="gold")

# spending vs. income
intercept1 <- mean(beta1.final[,1])
intercept0 <- mean(beta0.final[,1])
gender.coef1 <- mean(beta1.final[,2])
gender.coef0 <- mean(beta0.final[,2])
age.coef1 <- mean(beta1.final[,3])
age.coef0 <- mean(beta0.final[,3])
income.coef1 <- mean(beta1.final[,4])
income.coef0 <- mean(beta0.final[,4])
avg.age <- mean(mall$age)
x <- ppoints(1000)*140
y0 <- x * income.coef0 + intercept0 + gender.coef0 + age.coef0 * avg.age 
y1 <- x * income.coef1 + intercept1 + gender.coef1 + age.coef1 * avg.age 
plot(mall$annual_income, mall$spending, pch=20, main = "Spending Score vs. Annual Income", xlab = "Annual income", ylab = "Spending Score")
lines(x, y0, col="cornflowerblue", lwd=3)
lines(x, y1, col="gold", lwd=3)

# spending vs age
plot(mall$age, mall$spending, pch=20, main = "Spending Score vs. Age", xlab = "Age", ylab = "Spending Score")
avg.income <- mean(mall$annual_income)
x <- ppoints(1000)*70
y0 <- x*age.coef0 + intercept0 + gender.coef0 + avg.income*income.coef0
y1 <- x*age.coef1 + intercept1 + gender.coef1 + avg.income*income.coef1
lines(x,y0,col="cornflowerblue", lwd=3)
lines(x, y1, col="gold", lwd=3)

# predicted value
group.idx = ifelse(mean.inds>=0.5, 1, 0) 
X1 <- X[group.idx == 1,]
X0 <- X[group.idx == 0, ]
pred.y1 <- X1$gender * gender.coef1 + X1$age * age.coef1 + X1$annual_income * income.coef1 + intercept1
pred.y0 <- X0$gender * gender.coef0 + X0$age * age.coef0 + X0$annual_income * income.coef0 + intercept0
abs.diff <- c(abs(y[group.idx==1]-pred.y1), abs(y[group.idx==0]-pred.y0))
mean.abs.diff <- mean(abs.diff)

mall["male"] = ifelse(mall$gender=="Male", 1, 0)
reg.model <- lm(spending~male+age+annual_income, data=mall)
pred.ys <- predict(reg.model)
abs.diff2 <- abs(pred.ys - y)
mean.abs.diff2 <- mean(abs.diff2)