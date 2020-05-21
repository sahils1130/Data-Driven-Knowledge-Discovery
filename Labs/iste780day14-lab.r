library(ISLR)
options(digits=5)
attach(Wage)
fit <- lm(wage~poly(age,4),data=Wage)
names(summary(fit))
coef(summary(fit))
fit2 <- lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(summary(fit2a))
fit2b <- lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(summary(fit2b))
## plot
agelims  <- range(age)
age.grid <- seq(from=agelims[1],to=agelims[2])
preds    <- predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
# now construct Figure 7.1
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,40))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
##
preds2 <- predict(fit2,newdata=list(age=age.grid),se=T)
max(abs(preds$fit-preds2$fit))
## ANOVA of nested models
fit.1 <- lm(wage~age,data=Wage)
fit.2 <- lm(wage~poly(age,2),data=Wage)
fit.3 <- lm(wage~poly(age,3),data=Wage)
fit.4 <- lm(wage~poly(age,4),data=Wage)
fit.5 <- lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
coef(summary(fit.5))
## a different ANOVA
fit.1 <- lm(wage~education+age,data=Wage)
fit.2 <- lm(wage~education+poly(age,2),data=Wage)
fit.3 <- lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
## predict whether wage>250k
fit   <- glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds <- predict(fit,newdata=list(age=age.grid),se=TRUE)
pfit  <- exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit  <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))
## alternative; not used
preds <- predict(fit,newdata=list(age=age.grid),type="response",se=TRUE)
## back to plotting
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
## step function
table(cut(age,4))
fit <- lm(wage~cut(age,4),data=Wage)
options(digits=3)
coef(summary(fit))
##
## splines
##
library(splines)
## regression spline
fit <- lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred <- predict(fit,newdata=list(age=age.grid),se=TRUE)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
## natural spline
fit2 <- lm(wage~ns(age,df=4),data=Wage)
pred2 <- predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)
## smoothing spline, Figure 7.8
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Splines")
fit <- smooth.spline(age,wage,df=16)
fit2 <- smooth.spline(age,wage,cv=T)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"), col=c("red","blue"),lty=1,lwd=2,cex=.8)
##
## local regression
##
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit <- loess(wage~age,span=.2,data=Wage)
fit2 <- loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
##
## GAMs
##
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
library(gam)
education
gam.m3 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)
# Figure 7.12
dev.off()
par(mfrow=c(1,3))
plot(gam.m3,se=T,col="blue")
# Figure 7.11
plot.gam(gam1,se=T,col="red")
## Should we use year? If so, how should we model it?
gam.m1 <- gam(wage~s(age,5)+education,data=Wage)
gam.m2 <- gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
## We should use a linear model for year.
plot(gam.m2,se=T,col="black")
summary(gam.m3)
## Make a prediction.
preds <- predict(gam.m2,newdata=Wage)
## Try local regression.
gam.lo <- gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo,se=T,col="green")
# local regression with interaction terms
gam.lo.i <- gam(wage~lo(year,age,span=0.5)+education,data=Wage)
library(akima)
plot(gam.lo.i)
# logistic regression GAM
gam.lr <- gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
# Figure 7.13
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
# What is with the hs dropouts?
table(education,I(wage>250))
# Are there really 3000?
dim(Wage)
268 + 966 + 643 + 663 + 381
# Guess so.
#
# GAM without the hs dropouts
gam.lr.s <- gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
# Figure 7.14
plot(gam.lr.s,se=TRUE,col="green")
