#'#### 1 Download the Social_Network_Ads dataset  and import it into #####

dataset = read.csv("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/TD3/Social_Network_Ads.csv",header=T,na.strings ="?")

#'#### 2. Explore and Describe the dataset (you can use str() and summary() functions, you can calculate and visualize the correlations, show some histograms, scatterplots, pie charts, etc..).#####

str(dataset)

#'data.frame':	400 obs. of  5 variables:
#  $ User.ID        : int  15624510 15810944 15668575 15603246 15804002 15728773 15598044 15694829 15600575 15727311 ...
#$ Gender         : Factor w/ 2 levels "Female","Male": 2 2 1 1 2 2 1 1 2 1 ...
#$ Age            : num  19 35 26 27 19 27 27 32 25 35 ...
#$ EstimatedSalary: num  19000 20000 43000 57000 76000 58000 84000 150000 33000 65000 ...
#$ Purchased      : int  0 0 0 0 0 0 0 1 0 0 ...
 
summary(dataset)

#User.ID            Gender         Age        EstimatedSalary    Purchased     
#Min.   :15566689   Female:204   Min.   :18.00   Min.   : 15000   Min.   :0.0000  
#1st Qu.:15626764   Male  :196   1st Qu.:29.75   1st Qu.: 43000   1st Qu.:0.0000  
#Median :15694342                Median :37.00   Median : 70000   Median :0.0000  
#Mean   :15691540                Mean   :37.66   Mean   : 69743   Mean   :0.3575  
#3rd Qu.:15750363                3rd Qu.:46.00   3rd Qu.: 88000   3rd Qu.:1.0000  
#Max.   :15815236                Max.   :60.00   Max.   :150000   Max.   :1.0000  

cor_Age_Purchased = cor(dataset$Age,dataset$Purchased)
#[1] 0.6224542 high positive correlation

plot(dataset$Age)
#The subjects are mostly between 20 and 60 years old, and are equally reparted

#'#### 3. Now we are going to split the dataset into training set and test set. Last week we did it manually. From now on we will split it randomly, you can use this code (after undestanding it of course #####

library(caTools) # install it first in the console
set.seed(123)
# we use the function set.seed() with the same seed number
# to randomly generate the same values, you already know that right? 
#and you know why we want to generate the same values, am I wrong? 
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
# here we chose the SplitRatio to 75% of the dataset,
# and 25% for the test set.
training_set = subset(dataset, split == TRUE)
# we use subset to split the dataset
test_set = subset(dataset, split == FALSE)

#'#### 4. Scale the input variables in both training set and test set. Do you know what is scaling? Explain it one sentence. #####

training_set$Age <- scale(training_set$Age)
training_set$EstimatedSalary <- scale(training_set$EstimatedSalary)
#training_set$Purchased <- scale(training_set$Purchased)
test_set$Age <- scale(test_set$Age)
test_set$EstimatedSalary <- scale(test_set$EstimatedSalary)
#test_set$Purchased <- scale(test_set$Purchased)

#'#### 5. Now fit a simple logistic regression model of Purchased in function of Age #####

model <- glm(Purchased ~ Age, family = "binomial", data = training_set)
summary(model)

#'#### 6 As you saw in the Logistic Regression chapter and in the previous question, we choose argument family to be binomial when we use the function glm. Explain why #####

#in order to tell R to run a logistic regression rather than some other type of generalized linear model


#'#### 7 Write in a equation the model you obtained in question 5? (Read the following note concerning this question) #####

#P(purchased = 1/temperature = x) = logistic(B0 + B1x) = 1 / (1 + e^-(B0 + B1x))
#x <- seq(-2, 2, l = 200)
#y <- exp(-(model$coefficients[1] + model$coefficients[2] * x))
#y <- 1 / (1 + y)

eq <- function(x){1/(1+exp(-(model$coefficients[1] + model$coefficients[2] * x)))}

#'#### 8 Is the feature Age significant to the model? Justify your answer. #####
summary(model)
# the p-value for the feature Age is equal to 2e-16 < 5% so yes, the feature Age is significant to the model

#'#### 9 What is the value of AIC of the model? #####
#AIC : 256.11

#'#### 10  Plot Purchased in function of Age and add the curve of the obtained logistic regression model. #####


plot((training_set$Age), training_set$Purchased,
     xlab= "Age",
     ylab= "Purchased",
     pch=20)

curve(eq , add = TRUE)

#'#### 11 Now let us take another feature into account in the model. #####
# Fit a logistic regression model of purchasing the product in function of the age of a user and its salary.

model2 <- glm(Purchased ~ Age+EstimatedSalary, family = "binomial", data = training_set)
summary(model2)

#'#### 12 Are the predictors significant to the new model? ####
# the p-values are way below 0.05 (5%) so the predictors are significant

#'#### 13 Do we obtain a better model by adding the estimated salary? #####
# the p-values of the predictors are still way below 0.05 (5%) so the model is still significant, but the p-value 
# of the Salary is higher 2.03e-09>2.83e-14 , so it's not a better model

#'#### 14  On the test set, predict the probability of purchasing the product by the users using the obtained model. #####
pred_values <- predict(model2, newdata=test_set, type = 'response')

#'#### 15 Take a look on your predicted values for the variable Purchased. We predicted the probability that the user will purchase the product right? Now in order to compare your results with the real answers, transform the predicted values to 0 or 1 (1 if >0.5) #####
pred_values <- ifelse(pred_values>0.5,1,0)

#'#### 16 Now in order to evaluate the model and its predictions, compute the confusion matrix. What do you obtain ? #####
library(caret)

true_condition <- table(pred_values)
predict_condition <- table(test_set$Purchased)

#xtab<- table(predict_condition,true_condition)
#confusionMatrix(predict_condition, true_condition)


conf_mat = table(test_set$Purchased,pred_values > 0.5)

#     [,1] [,2]
#[1,]   57    7
#[2,]   10   26

#'#### 17 Calculate the accuracy, specificity, sensitivity and the precision of the model. #####

accuracy <- (conf_mat[1]+conf_mat[4])/(sum(conf_mat))
#0.83

specificity <- (conf_mat[1])/(conf_mat[1] + conf_mat[3])
#0.890625

sensitivity <- (conf_mat[4])/(conf_mat[2] + conf_mat[4])
#0.722222

precision <- (conf_mat[1])/(conf_mat[1] + conf_mat[4])
#0.686747

#'#### 18 Plot the ROC curve and calculate AUC value #####

library(ROCR)
pred_model2 = prediction(pred_values,test_set$Purchased)
AUC_2 = performance(pred_model2,"tpr","fpr")
plot(AUC_2,col="coral")
abline(0,1,col = "lightgrey")

#'#### 19. Compare the AUC of the two models you fitted (one with only age and one with age and estimated salary) and plot their ROC curves in the same figure.

predicted_values1 = predict(model2,newdata=test_set,type="response")
pred_model1 = prediction(predicted_values1,test_set$Purchased)
AUC_1 = performance(pred_model1,"tpr","fpr")
plot(AUC_1,col="seagreen")
lines(as.numeric(unlist( AUC_2@x.values )),
      as.numeric(unlist( AUC_2@y.values )),
      col="coral")
abline(0,1,col="lightgrey")