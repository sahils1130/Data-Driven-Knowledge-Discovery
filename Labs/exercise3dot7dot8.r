## Exercise 3.7.8
## Mick McQuaid
## Summer 2016

# 8. This question involves the use of simple linear regression on the Auto data set.

options(width=60)
Auto  <- read.csv("Auto.csv",header=T,na.strings="?")
Auto  <- na.omit(Auto)
nrow(Auto)
attach(Auto)

# (a) Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the summary() function to print the results.

lm(mpg ~ horsepower)
summary(lm(mpg ~ horsepower))
lmMpgHp <- lm(mpg ~ horsepower)
summary(lmMpgHp)

# Comment on the output. For example:
# 
# i. Is there a relationship between the predictor and the re- sponse?

## Yes, as evidenced by the large t-statistic associated
## with $\hat\beta_1$ also known as the Estimate or
## Coefficient for horsepower.

# ii. How strong is the relationship between the predictor and the response?

## The relationship is fairly strong as evidenced by the
## R^2=0.6

# iii. Is the relationship between the predictor and the response positive or negative?

## The relationship is negative as evidenced by the
## minus sign in front of -0.157845, the Estimate for
## horsepower.

# iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?

## 39.94+(98*-0.16) = approx 25 mpg

# (b) Plot the response and the predictor. Use the abline() function to display the least squares regression line.

plot(horsepower,mpg)
abline(lmMpgHp)

# (c) Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit.

par(mfrow=c(2,2))
plot(lmMpgHp)
plot(hatvalues(lmMpgHp))
which.max(hatvalues(lmMpgHp))

## The residuals show a pattern indicating we've left
## something out. The QQ plot tells us that the
## residuals are not normally distributed. The leverage
## statistic can range between 1/n (0.003 in this case)
## and 1 (actually 0.03 in this case). We expect it
## on average to be (p+1)/n or 0.005 and we expect
## Cook's distance for any point to be below 4/n, or
## about 0.01, so we have about twenty points with high
## leverage.

