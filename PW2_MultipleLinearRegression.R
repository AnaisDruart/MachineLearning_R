#1
library(MASS)
dim(Boston)
Boston

#2
# Split the data by using the first 400 observations as the training
# data and the remaining as the testing data
train = 1:400
test = -train

training_data = Boston[train, names(Boston)]
testing_data = Boston[test, names(Boston)]


#3
#Check if there is a linear relationship between the var medv and age
cor(training_data$age,training_data$medv, use="everything", method = c("pearson","kendall", "spearman"))
#Pearson mesure une correlation lineaire

#4
#Fit a model of housing prices in function of age and plot the observations and the regression line.

plot((training_data$age), training_data$medv,
     xlab= "Age of the house",
     ylab= "Median of house",
     col="blue",
     pch=20)

model = lm(medv ~ age, data = training_data)
model
summary(model) 

# Add the regression line to the existing scatterplot
abline(model,col="red", lwd=2)

#5
#Train a regression model using both lstat and age as predictors of median house value. 
#(Remember that we transformed lstat, use the same transformation here). 
#What is the obtained model?

model2 = lm(medv ~ log(lstat)+age, data = training_data)
model2
#the regression model is a plan and not an equation
#install packages ("rgl)
library(rgl)
rgl::plot3d(log(Boston$lstat), Boston$age, Boston$medv, type = "p",
            xlab = "log(lstat",
            ylab = "age",
            zlab = "medv",
            site = 5,
            col = "red",
            lwd = 15)
rgl.snapshot("rgl_2_R.gif") #to save a snapshot
#the regession model is a plane

a = model2$coefficients["log(lstat"]
b = model2$coefficients["age"]
c=-1
d=model2$coefficients["Intercept"]
rgl::planes3d(a,b,c,d,apha=0.3,front="line")

#6
# Print the summary of the obtained regression model.
summary(model2)


#7 Is the model as a whole significant? Answer on this question must be detailed.
#the global p-value = 2.2e-16 <5% so the test of Fisher confirm the validity of the model
# The multiple R-squared = 0.6684, so the proportion of predictors Xi to explain Y is closer from 1 than 0, and the model 
#explains a large portion of the variance in the response variable

#8
#Are the predictors significant ?
#oui car toutes les p values sont inferieures à 5%  -> the model proposed is significant since all the estimated coefficients are significative ( all p-values <5%)



#9 Train a new model using all the variables of the dataset. (We can use . as a short cut instead of writing down all the variables names)
model3 <- lm(medv ~ . , data = training_data)
summary(model3)


# 10 When using all the variables as predictors
#we didn’t transform lstat. Re train the model using log(lstat)
#instead of lstat.
model4 <- lm(medv ~ . -lstat+log(lstat), data = training_data)
summary(model4)

#11 Did  R2 improve ?

summary(model3)$r.squared
summary(model4)$r.squared
# R2 is better with the model with the log
#or
#the residual standard err is the stand var
#summary(model4)$.r.squared > summary(model3)$r.squared

#12 To see if there is correlated variables print the correlation matrix using the cor() function (round the correlations with 2 digits).
round(cor(training_data),2)

#13 Visualize the correlations using the corrplot package. To do so, install the corrplot package, load it, then use the function corrplot.mixed()
library(corrplot)
corrplot.mixed(cor(training_data))

#14 What is the correlation between tax and rad?
#0.87 high correlation

#15 Run the model again without tax. What happens to the  R2 ? and for the F-statistic?

model5 = lm(medv ~ . -lstat + log(lstat) - tax, data=training_data)
summary(model5)
summary(model5)$r.squared < summary(model4)$r.squared
#R2 is lower, but it's normal, since we deleted one of the variables
#the model significance F-statistic is higher, which means the p-values gets lower
#so, the model is more significant without tax

#16Calculate the mean squared error (MSE) for the last model.

#vector = y - y_hat (error obtained stocked in a vector)
# we do the square of the vector
# at the end we evaluate th mean
y = testing_data$medv
y_hat = predict(model5, data.frame(testing_data))
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE

#17 In the Boston data set there is a categorical variable chas which corresponds to Charles River 
#(= 1 if a suburb bounds the river; 0 otherwise). 
#Use command str() to see how this variable is present in the dataset. 
#How many of the suburbs in this data set bound the Charles river
str(training_data)
table(training_data$chas) #35
#or
sum(training_data$chas == 1)
#35

#18Create Boxplots of the median value of houses with respect to the variable chas. 
#Do we observe some difference between the median value of houses with respect to the neighborhood to Charles River
boxplot(medv~chas, data=training_data)
#yes, we do : the median value of houses is higher in the neighborhood to Charles River

#19 Calculate μi and μj (in one line using the function aggregate()

aggregate(formula=medv~chas, data=training_data, FUN=mean)
#ans 0 23.94082
#ans 1 28.44000
#the price is higher in the neighbourhood of Charles River

#20 Apply an ANOVA test of medv whith respect to chas (use the function aov()). Print the result and the summary of it. what do you conclude 
anovatest = aov(medv~ chas, data = training_data)
anovatest
summary(anovatest)
#Significative difference of price if close to Charles River or not

#21Fit a new model where the predictors are the Charles River and the Crime Rate. 
#Interpret the coefficients of this model and conclude if the presence of the river adds a valuable information for explaining the house price.
model6 = lm(medv~chas + crim, data=training_data)
summary(model6)
#it does add a valuable information: : just by looking at the significant codes, we can see that the model6 has way more starts than the other model

#22 Is chas is significant as well in the presence of more predictors
#it's less significant with more variables

#23 Fit a model whith first order interaction term where predictors are lstat and age. Print its summary.
model7=lm(medv~lstat:age, data=training_data)
summary(model7)

#24 Fit a model with all the first order interaction terms.
model8 = lm(medv~.:., data=training_data)
summary(model8)