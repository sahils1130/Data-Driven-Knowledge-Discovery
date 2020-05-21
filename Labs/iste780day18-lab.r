library(e1071)
set.seed(1)
# Create some example data
x <- matrix(rnorm(20*2),ncol=2)
# x is now normally distributed with 40 obs in a 20x2 matrix
y <- c(rep(-1,10),rep(1,10))
# y is now a vector consisting of ten copies of negative
# one followed by ten copies of positive one
#
# Look at what we've created
x
y
x[y==1,] <- x[y==1,]+1
# Are they separable?
plot(jitter(x),col=(3-y))
# Nope, close but not quite
#
# Create a data frame with x and y. Notice that R lets x
# and y that we have created appear on the right side of
# the parameter assignment statements without confusing
# them with the parameter names, which can only appear
# on the left hand side of the assignments.
dat <- data.frame(x=x,y=as.factor(y))
# Fit a linear support vector classifier
svmfit <- svm(y~.,data=dat,kernel="linear",cost=10,scale=F)
plot(svmfit,dat)
svmfit$index
dat
summary(svmfit)
# Now change the cost
svmfit <- svm(y~.,data=dat,kernel="linear",cost=0.1,scale=F)
plot(svmfit,dat)
svmfit$index
# Smaller C leads to larger margin and more support
# vectors.
#
# Time to do cross-validation!
set.seed(1)
tune.out <- tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
# tune() can take a range of C values we want to try;
# note that we try seven values.
summary(tune.out)
# Turns out 0.1 has lowest error rate
#
# tune() stores the best model in tune.out$best.model
names(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
# Create test data
xtest <- matrix(rnorm(20*2),ncol=2)
ytest <- sample(c(-1,1),20,rep=T)
xtest[ytest==1,] <- xtest[ytest==1,]+1
testdat <- data.frame(x=xtest,y=as.factor(ytest))
ypred <- predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
# Good job! 19/20 correct predictions
#
# Try it with cost=0.01 instead to see if
# cross-validating really made a difference.
svmfit <- svm(y~.,data=dat,kernel="linear",cost=0.01,scale=F)
ypred  <- predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)
# Results slightly worse: (11+7)/20
#
# Now try a linearly separable example.
x[y==1,] <- x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)
dat <- data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)
# Now try with much smaller cost
svmfit <- svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)
#
# Support vector machine
#
set.seed(1)
x <- matrix(rnorm(200*2),ncol=2)
x[1:100,] <- x[1:100,]+2
x[101:150,] <- x[101:150,]-2
y <- c(rep(1,150),rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))
plot(x,col=y)
# Split off a training data set
#
# Remember this notation? It means sample size of 100
# from among integers 1 to 200 without replacement.
train <- sample(200,100)
train
svmfit <- svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
# Expect increasing the cost to ruin our nice egg-shaped
# region.
svmfit <- svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
# Yes, we now have a very bizarre shape.
#
# Why not try cross-validation? It should be habitual by
# now!
set.seed(1)
tune.out <- tune(svm,y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
# My results differ from those of the textbook.
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))
#(69+15)/(69+3+13+15)
(74+16)/(74+3+7+16)
# My results (84 percent) are decidedly worse than the textbook
# results of 90 percent.
#
# ROC curves
#
# Note library(ROCR) is case sensitive.
library(ROCR)
rocplot <- function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}
# Output fitted values with decision.values=T
svmfit.opt <- svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted <- attributes(predict(svmfit.opt,dat[train,],decision.values=T))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
# Try increasing gamma for a more flexible model
svmfit.flex <- svm(y~.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values=T)
fitted <- attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
# Now try the test data
fitted <- attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted <- attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")
#
# SVM with multiple classes
#
# First, we add a third class to existing x and y
set.seed(1)
x <- rbind(x,matrix(rnorm(50*2),ncol=2))
y <- c(y,rep(0,50))
x[y==0,2] <- x[y==0,2]+2
dim(x)
length(y)
dat <- data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
# See the three classes and how they overlap?
svmfit <- svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)
#
# Application to gene expression data
#
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat <- data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out <- svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y)
# No training data errors!
#
# Let's try the test data
dat.te <- data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te <- predict(out,newdata=dat.te)
table(pred.te,dat.te$y)
# Still not bad, though not as good as training!
