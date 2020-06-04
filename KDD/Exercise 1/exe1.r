##Exercise 2.4.9
##Sahil Shah
##Summer 2020

# 9. This exercise involves the Auto data set studied in the lab. Make sure
# that the missing values have been removed from the data.
require(ISLR)
data(Auto)
data.frame(Auto)
str(Auto)
Auto <- na.omit(Auto)
nrow(Auto)
ncol(Auto)
colnames(Auto)
head(Auto)
attach(Auto)
cylinders <- as.factor(cylinders)

# (a) Which of the predictors are quantitative, and which are qualitative?

summary(Auto)

##Quantitative data : mpg, displacement, horsepower, weight, accelaration, year
##Qualitative data : name, origin, cylinders

# (b) What is the range of each quantitative predictor? You can answer this using the range() function.
# range()

range(mpg)
sapply(Auto[,c(1,3:7)], range)

# (c) What is the mean and standard deviation of each quantitative
# predictor?
sapply(Auto[,c(1,3:7)], mean)
sapply(Auto[,c(1,3:7)], sd)
as.data.frame(t(sapply(Auto[,c(1,3:7)], function(test) list(Means = mean(test), sds = sd(test),
                                                            ranges = range(test)))))

# (d) Now remove the 10th through 85th observations. What is the
# range, mean, and standard deviation of each predictor in the
# subset of the data that remains?
as.data.frame(t(sapply(Auto[-10:-85,c(1,3:7)], function(test) list(Means = mean(test), sds = sd(test),
                                                            ranges = range(test)))))

  
# (e) Using the full data set, investigate the predictors graphically,
# using scatterplots or other tools of your choice. Create some plots
# highlighting the relationships among the predictors. Comment
# on your findings.
pairs(Auto)
plot(displacement, horsepower)
plot(mpg,weight)
# 
# (f) Suppose that we wish to predict gas mileage (mpg) on the basis
# of the other variables. Do your plots suggest that any of the
# other variables might be useful in predicting mpg? Justify your
# answer.