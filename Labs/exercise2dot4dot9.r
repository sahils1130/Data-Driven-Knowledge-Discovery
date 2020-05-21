## Exercise 2.4.9
## Mick McQuaid
## Summer 2016

# 9. This exercise involves the Auto data set studied in the lab. Make sure that the missing values have been removed from the data.

Auto <- read.csv("Auto.csv",header=T,na.strings="?")
Auto <- na.omit(Auto)
nrow(Auto)
ncol(Auto)
options(width=50)
colnames(Auto)
attach(Auto)
cylinders <- as.factor(cylinders)

# (a) Which of the predictors are quantitative, and which are quali- tative?

summary(Auto)

## Quantitative: mpg, displacement, horsepower, weight,
## acceleration, year
## Qualitative: cylinders, origin, name

# (b) What is the range of each quantitative predictor? You can an- swer this using the range() function.

range(mpg)
range(displacement)
sapply(Auto[,c(1,3:7)],range)

# (c) What is the mean and standard deviation of each quantitative predictor?

sapply(Auto[,c(1,3:7)],mean)
sapply(Auto[,c(1,3:7)],sd)
as.data.frame(t(sapply(Auto[,c(1,3:7)],function(bla) list(means=mean(bla),sds=sd(bla),ranges=range(bla)))))

# (d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?

sapply(Auto[-10:-85,c(1,3:7)],range)
sapply(Auto[-10:-85,c(1,3:7)],mean)
sapply(Auto[-10:-85,c(1,3:7)],sd)
as.data.frame(t(sapply(Auto[-10:-85,c(1,3:7)],function(bla) list(means=mean(bla),sds=sd(bla),ranges=range(bla)))))

# (e) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.

pairs(Auto)
plot(displacement,horsepower)

## The strongest linear relationship is between
## displacement and horsepower. The next strongest
## linear relationships seem to be between displacement
## and weight, horsepower and weight. There is a strong
## categorical relationship between displacement and
## cylinders. There is a somewhat strong categorical
## relationship between displacement and origin. There
## are strong curvilinear relationships between mpg and
## displacement, mpg and horsepower, and mpg and weight.
## The other relationships are less clear.

# (f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.

## Weight appears to be the best predictor of mpg. A
## scatterplot shows that only two 3500 lb+ cars get
## better than 20 mpg, while no 40 mpg+ car weighs more
## than 2500 lbs. Most cars show a relationship between
## mpg and weight on a smooth curve. Displacement and
## horsepower also appear to be good predictors,
## especially for larger values of displacement and
## horsepower.
