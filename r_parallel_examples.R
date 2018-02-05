#######
#
# R parallel computing tutorial
#
# by: Jeff Minucci
#
#######


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
iris <- droplevels(iris[iris$Species != "setosa",]) #using only two species (binomial classification problem)
head(iris)
summary(iris$Species)


#Let's fit a logistc regression to this data to predict the species of iris based on its flower shape
fit <- glm(Species ~ ., data=iris, family=binomial(logit))
summary(fit)

#And plot a significant effect
plot(iris$Petal.Length, as.numeric(iris$Species)-1,pch=16,xlab="Petal length",ylab="Probability of virginica")
lengths <- seq(3,7,by=0.01)
newdata <- data.frame(Sepal.Length = mean(iris$Sepal.Length),Sepal.Width = mean(iris$Sepal.Width), 
                      Petal.Length = lengths, Petal.Width = mean(iris$Petal.Width))
spp_predict <- predict(fit,newdata,type="response")
lines(lengths,spp_predict,lwd=3,col="black")

#Now let's "bootstap" some 95% confidence intervals by sampling our data 100 times and fitting the same function
#Note: bootstapping involves randomly resampling the data with replacement 



