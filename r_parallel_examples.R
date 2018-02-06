#######
#
# R parallel computing tutorial
#
# by: Jeff Minucci
#
#######
library(boot)

#First load the necessary library 
library(doParallel)

#Start a cluster (Windows)
c1 <- makeCluster(4)
registerDoParallel(c1)

#Get info about our parallel backend
getDoParWorkers()


#### Let's start with a trivial example:
#### taking a bunch of square roots

#With a traditional for loop
result <- rep(0,10^4)
for(i in 1:10^4){
  result[i] <- sqrt(i)
}



#With a foreach loop  - notice .combine = 'c' is telling it to form our output using the c function
time <- system.time({
  result <- foreach(i=1:10^4, .combine='c') %do% {
    sqrt(i)
  }
})[3]
time


#Another example, this time using 'cbind' as the combine function
x <- foreach(i=1:10, .combine='cbind') %do% rnorm(4)
x


#Note: the default .combine is to create a list of outputs


#Back to our sqrt example... using a foreach loop - in parallel
time <- system.time({
  result <- foreach(i=1:10^4, .combine='c') %dopar% {
    sqrt(i)
  }
})[3]
time


#Why didn't we see a speed improvement?


#Of course the sqrt function is vectorized, so we would never want to loop through it anyways...
time <- system.time({
  result <- sqrt(1:10^4)
})[3]
time


########################

#Now let's look at a more practical example where parallel processing is actually efficient:
#Bootstrapping a logistic regression:

#We will use the 'iris' example dataset
data(iris)
iris <- droplevels(iris[iris$Species != "setosa",c(1,5)]) #using only two species (binomial classification problem)
head(iris)
summary(iris$Species)



#Let's fit a logistc regression to this data to predict the species of iris based on its flower shape
fit <- glm(iris[,2]~iris[,1], family=binomial(logit))
summary(fit)

#And plot a significant effect
plot(iris[,1], as.numeric(iris[,2])-1,pch=16,xlab="Sepal Length",ylab="Probability of virginica")
lengths <- seq(4,9,by=0.01)
spp_predict <- inv.logit(lengths*coefficients(fit)[2] + coefficients(fit)[1])
lines(lengths,spp_predict,lwd=3,col="black")



#Now let's "bootstap" some 95% confidence intervals by sampling our data 10k times and fitting the same function. 
#Then we can calculate 2.5th and 97.5th percentile for our 'petal length' effect/

time <- system.time({
  nsims <- 10000
  x <- foreach(i=1:nsims, .combine=rbind) %dopar% {
    boot_sam <- sample(100,100,replace=T) #randomly sample 100 row #s out of our 100 row dataset (with replacement)
    boot_fit <- glm(iris[boot_sam,2]~iris[boot_sam,1], family=binomial(logit))
    coefficients(boot_fit)
  }
})[3]
time

#compare to time for non-parallel:
time <- system.time({
  nsims <- 10000
  x <- foreach(i=1:nsims, .combine=rbind) %do% {
    boot_sam <- sample(100,100,replace=T) #randomly sample 100 row #s out of our 100 row dataset (with replacement)
    boot_fit <- glm(iris[boot_sam,2]~iris[boot_sam,1], family=binomial(logit))
    coefficients(boot_fit)
  }
})[3]
time



#Plot bootsrapped 95% confidence intervals
hist(x[,2])
CIs <- quantile(x[,2],c(.025,.975))
plot(iris[,1], as.numeric(iris[,2])-1,pch=16,xlab="Sepal length",ylab="Probability of virginica")
lengths <- seq(3,9,by=0.01)
spp_predict <- inv.logit(lengths*coefficients(fit)[2] + coefficients(fit)[1])
low_predict <-  inv.logit(lengths*CIs[1] + coefficients(fit)[1])
up_predict <-  inv.logit(lengths*CIs[2] + coefficients(fit)[1])
lines(lengths,spp_predict,lwd=3,col="black")
lines(lengths,low_predict,lwd=3,col="grey",lty=4)
lines(lengths,up_predict,lwd=3,col="grey",lty=4)


results <- boot(data=iris,statistic)
