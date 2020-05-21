system("clear")
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
# pcr on training subset, evaluated with test subset
set.seed(1)
pcr.fit <- pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
# find the test MSE
pcr.pred <- predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
# Above shows that MSE is competitive with ridge and lasso;
# Unfortunately it is hard to interpret results because they
# are linear combinations of the predictors!
#
# pcr on full data set
pcr.fit <- pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)
#
# pls
set.seed(1)
pls.fit <- plsr(Salary~.,data=Hitters,subset=train,scale=T,validation="CV")
summary(pls.fit)
# lowest cv error occurs when M=2, so check
# corresponding test MSE
pls.pred <- predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
# test MSE is in the same ballpark as those for ridge
# regression, the lasso, and PCR
pls.fit <- plsr(Salary~.,data=Hitters,scale=T,ncomp=2)
summary(pls.fit)
# notice how two-component pls explains nearly as much
# of salary (46.40 percent) as does the seven-component
# PCR model---textbook points out that this is because
# PCR is only looking for variability among predictors
# while PLS looks at both predictors and response
