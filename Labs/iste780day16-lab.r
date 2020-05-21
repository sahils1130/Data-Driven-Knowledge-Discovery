library(tree)
library(ISLR)
attach(Carseats)
names(Carseats)
High <- ifelse(Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats,High)
tree.carseats <- tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
## Estimate test error
set.seed(2)
train <- sample(1:nrow(Carseats),200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales,Carseats,subset=train)
tree.pred <- predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
## We made correct predictions for this fraction of the data:
(86+57)/200
## Use cross validation to identify optimal tree complexity
set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass)
## prune.misclass means use classification error rate to
## guide cv and pruning instead of default deviance
names(cv.carseats)
## k here is equivalent to alpha in the textbook
cv.carseats
## dev here is actually classif error rate
## lowest (50) for tree with nine nodes!
##
## Plot error rate as function of size and alpha (k)
par(mfrow=c(1,2))
## dev is classification error rate in following plot
plot(cv.carseats$size,cv.carseats$dev,type="b")
## k is alpha from the textbook in following plot
plot(cv.carseats$k,cv.carseats$dev,type="b")
## Prune the tree to the best subtree
prune.carseats <- prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
## Test the pruned tree
tree.pred <- predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
## The pruned tree correctly classifies the following
## fraction of test observations
(94+60)/200
## Try the same process with a larger tree
prune.carseats <- prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
## Test the pruned tree
tree.pred <- predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
## The larger tree correctly classifies the following
## fraction of test observations
(86+62)/200
##
## Regression trees
##
library(MASS)
set.seed(1)
## Create training data and fit tree to it
train  <- sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston,subset=train)
summary(tree.boston)
names(Boston)
## Only three of thirteen variables were used
##
## Plot the tree
plot(tree.boston)
text(tree.boston,pretty=0)
plot(tree.boston)
text(tree.boston,pretty=1)
## pretty=0 makes no difference on this one!
##
## Try pruning
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")
## Even though 8 is best, let's prune to 5 anyway
prune.boston <- prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
## Because cross-validation said not to prune, let's try
## the entire tree on the test data
yhat <- predict(tree.boston,newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
## Compute the test MSE
mean((yhat-boston.test)^2)
## Test MSE is about 25, indicating SD of about 5, so
## predictions from this model are within 5,000USD of
## the true median home value for the suburb
##
## Bagging, Random Forests
##
## Use the randomForest package for both bagging and RFs
## since bagging is a RF with m=p
library(randomForest)
set.seed(1)
## First try bagging
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
## Try it on the test data
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
## MSE is much smaller than for optimally-pruned tree!
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
## MSE is slightly higher
##
## Now try a random forest by reducing the size of mtry
set.seed(1)
rf.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=T)
yhat.rf <- predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
## My test MSE is slightly higher than that of the
## textbook. Why do you suppose that is? Probably a
## result of random sampling for the training / test
## data.
##
## Examine variable importance
options(digits=4)
importance(rf.boston)
## IncMSE is the mean decrease of accuracy in
## predictions on the out of bag samples when the given
## variable is excluded from the model.
## IncNodePurity measures the decrease in node impurity
## that results from splits over that variable, averaged
## over all trees; uses training RSS for regression and
## uses deviance for classification
##
## Plot the importance
varImpPlot(rf.boston)
## Result is that lstat and rm are by far most important
##
## Boosting
##
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
## Make partial dependence plots for lstat and rm
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
## Use boosted model to predict medv on test data
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
## Test MSE is similar to randomForest result and
## superior to bagging
##
## Try boosting with lambda = 0.2
boost.boston <- gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
## lambda = 0.2 gives a slightly lower test MSE than the
## default of lambda = 0.001
