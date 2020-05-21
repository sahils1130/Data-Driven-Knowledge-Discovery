#
# Lab 2: ridge regression and the lasso, page 251
#
system("clear")
x <- model.matrix(Salary~.,Hitters)[,-1]
y <- Hitters$Salary
library(glmnet)
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm
predict(ridge.mod,s=50,type="coefficients")[1:20,]
# split the samples into training and test sets
set.seed(1)
train      <- sample(1:nrow(x),nrow(x)/2)
test       <- (-train)
y.test     <- y[test]
ridge.mod  <- glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred <- predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred <- predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# make it equivalent to least squares via lambda=0
ridge.pred <- predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
# use k-fold cv with k=10 to choose lambda
set.seed(1)
cv.out  <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# lasso
lasso.mod <- glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
