x <- c(1,3,2,5)
x
#ans> [1] 1 3 2 5

x = c(1,6,2)
x
#ans> [1] 1 6 2
y = c(1,4,3)
length(x)
#ans> [1] 3
length(y)
#ans> [1] 3
x+y
#ans> [1]  2 10  5

ls()
#ans> [1] "x"  "x1" "x2" "x3" "x4" "y"
rm(x)
ls()
#ans> [1] "x1" "x2" "x3" "x4" "y"


#### 1.2.8 VECTORS ######

x = c(1,7,3,4)
x
y = c(100:1)
y
x[3]+y[4]
cos(x[3])+sin(x[2]*exp(-y[2]))
x[3]=0
y[2]=-1
x
y
x[3]+y[4]
cos(x[3])+sin(x[2]*exp(-y[2]))

z = y[x+1]
z

##### 1.8.5 Distributions #####
df1 = 1
df2 = 5

qf(p=0.90,df1,df2)
qf(p=0.95,df1,df2)
qf(p=0.99,df1,df2)

rpois(100,5)

df1 = 1
x = seq(-4,4,l=100)
y = dt(x,df1)
z = dt(x,5)
a = dt(x,10)
b = dt(x,50)
c = dt(x,100)
plot(x,y, type ="l", col = "red")
lines(x,z, type = "l", col = "green")
lines(x,a, type = "l", col = "blue")
lines(x,b, type="l",col="pink")
lines(x,c, type="l",col="black")




##### 1.8.7 Loading data #########
Auto=read.table("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/TD1/Auto.data",header=T,na.strings ="?")


##### 1.9.1 ######
load("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/TD1/EU.RData")

myModel = lm(formula = CamCom2011 ~ Population2010, data = EU)
myModel$residuals
myModel$coefficients
summaryMyModel <- summary(myModel)
summaryMyModel$sigma

##### 1.9.2 Predicting House Value: Boston dataset #####

library(MASS)
dim(Boston)

# Split the data by using the first 400 observations as the training
# data and the remaining as the testing data
train = 1:400
test = -train

# Speficy that we are going to use only two variables (lstat and medv)
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

# Check the dimensions of the new dataset
dim(training_data)
#ans> [1] 400   2

plot(training_data$lstat, training_data$medv)
# Scatterplot of log(lstat) vs. medv
plot(log(training_data$lstat), training_data$medv)
model = lm(medv ~ log(lstat), data = training_data)
model
#ans> 
#ans> Call:
#ans> lm(formula = medv ~ log(lstat), data = training_data)
#ans> 
#ans> Coefficients:
#ans> (Intercept)   log(lstat)  
#ans>        51.8        -12.2

summary(model)
names(model)
model$coefficients
confint(model, level = 0.95)

# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv)

# Add the regression line to the existing scatterplot
abline(model)

# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv,
     xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
     ylab = "Median House Value",
     col = "red",
     pch = 20)

# Make the line color blue, and the line's width =3 (play with the width!)
abline(model, col = "blue", lwd =3)

predict(model, data.frame(lstat = c(5)))
#ans>    1 
#ans> 32.1
# Predict what is the median values of houses with lstat= 5%, 10%, and 15%
predict(model, data.frame(lstat = c(5,10,15), interval = "prediction"))
#ans>    1    2    3 
#ans> 32.1 23.7 18.7

# Save the testing median values for houses (testing y) in y
y = testing_data$medv

# Compute the predicted value for this y (y hat)
y_hat = predict(model, data.frame(lstat = testing_data$lstat))

# Now we have both y and y_hat for our testing data. 
# let's find the mean square error
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE
#ans> [1] 17.7