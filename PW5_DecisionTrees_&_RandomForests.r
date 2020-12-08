
#'# Regression Trees 
#'
#'## Single tree
#'
#'
#'
#'### 1 Load the Boston dataset from MASS package. Split the dataset randomly in half. 

library(MASS)
library(caTools)
set.seed(18)
Boston_idx = sample(1:nrow(Boston), nrow(Boston) / 2) 
Boston_train = Boston[Boston_idx,]
Boston_test  = Boston[-Boston_idx,]



#'###  2 Fit a regression tree to the training data using the rpart() function from the rpart package. Name the tree Boston_tree. 

library(rpart)
Boston_tree <- rpart(medv ~ ., data = Boston_train)



#'###  3 Plot the obtained tree using the following code. 

plot(Boston_tree)
text(Boston_tree, pretty = 0)
title(main = "Regression Tree")



#'###  4 A better plot can be obtained using the rpart.plot18 package. Re-plot the tree using it.  
#You can use the rpart.plot() function which by default, when the output is continuous, 
#each node shows: the predicted value, and the percentage of observations in the node. You can also use the prp() function.


library(rpart.plot)
# second visualization of the tree
rpart.plot(Boston_tree)
# third one
prp(Boston_tree)



#'###  5 Print the obtained tree and print its summary. Between the things that you can see in the summary, 
#the CP (complexity parameter) table and the importance of each variable in the model. 
#Print the CP table using the printcp() function to see the cross validation results. Plot a comparison figure using the plotcp() function.

Boston_tree
summary(Boston_tree)
printcp(Boston_tree)
plotcp(Boston_tree)

# Full grown tree, using cp = -1
Boston_full_tree <- rpart(medv ~ ., data = Boston_train, cp = -1)
rpart.plot(Boston_full_tree)
plotcp(Boston_full_tree)



#'###  6 Write a  Rfunction that returns the RMSE of two vector 



RMSE <- function(expected, observed)
{
  return(sqrt( mean( (expected-observed)^2  ) ))
}



#'###  7 Use the function predict() to predict the response on the test set. Then calculate the RMSE obtained with tree model.  


Boston_tree_predict <- predict(Boston_tree, Boston_test)
Boston_tree_RMSE = RMSE(Boston_tree_predict,Boston_test$medv)
Boston_tree_RMSE



#'###  8 Fit a linear regression model on the training set. Then predict the response on the test set using the linear model. 
#Calculate the RMSE and compare the performance of the tree and the linear regression model.

Boston_regression <- lm(medv ~ ., data = Boston_train)
Boston_regression_predict <- predict(Boston_regression, Boston_test)
Boston_regression_RMSE = RMSE(expected = Boston_regression_predict,observed = Boston_test$medv)
Boston_regression_RMSE
# We obtain a really slightly best RMSE with the linear model : 5.051138 (tree) vs 5.016083 (regression)

# Model performance plot
plot(Boston_tree_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     main = "Predicted vs Actual : Single Tree, Test Data")
abline(0,1,col='blue')

plot(Boston_regression_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     main = "Predicted vs Actual : Linear Model, Test Data")
abline(0,1,col='blue')


#'## Bagging


#'###  9 Fit a bagged model, using the randomForest() function from the randomForest package. 


library(randomForest)
p = length(Boston_train)-1
Boston_bagged <- randomForest(medv ~ ., data = Boston_train, mtry=p)



#'###  10 Predict the response on the test set using the bagging model. Calculate the RMSE.
#Is the performance of the model better than linear regression or a simple tree?

Boston_bagged_predict <- predict(Boston_bagged, Boston_test)
Boston_bagged_RMSE = RMSE(expected = Boston_bagged_predict,observed = Boston_test$medv)
Boston_bagged_RMSE

# We had 5.016083 for regression, 5.051138 for tree and now 3.889804 for bagging. So we have the best RMSE
# with bagging.

plot(Boston_bagged_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     main = "Predicted vs Actual : Bagging, Test Data")
abline(0,1,col='blue')

plot(Boston_bagged[["mse"]], type = 'l', 
     main = 'Bagged Trees: Error vs Number of Trees', 
     xlab='trees', 
     ylab='Error',
     lwd= 1)


#'## Random Forests



#'###  11 Fit a random forest on the training set and compare its performance with the previous models by calculating the predictions and the RMSE. 

p = length(Boston_train)-1
Boston_RandF <- randomForest(medv ~ ., data = Boston_train, mtry=(p/3))
Boston_RandF_predict <- predict(Boston_RandF, Boston_test)
Boston_RandF_RMSE = RMSE(expected = Boston_RandF_predict,observed = Boston_test$medv)
Boston_RandF_RMSE
# We had 5.016083 for regression, 5.051138 for tree 3.889804 for bagging and now 4.158363 for random forest.
# So we have the best RMSE with bagging.

plot(Boston_RandF_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     main = "Predicted vs Actual : Random Forest, Test Data")
abline(0,1,col='blue')



#'###  12 Use the function importance() from the randomForest package to see the most important predictors in the obtained random forest model. 
# What are the three most important predictors?
# Did you find the same results when you selected the best predictors for the linear regression model during session 2? 


importance(Boston_RandF)
# The IncNodePurity shows that the three best predictors are lstat : 6125.45876, rm : 5170.86256 and maybe indus : 1492.46951
# In PW2, we found lstat and age.




#'### 13 Plot the importance of the predictors to the model using the varImpPlot() function. 
varImpPlot(Boston_RandF,
          main = "Importance of the predictors to the random forest model")


#'## Boosting



#'###  14  Using the gbm() function like following, fit a boosted model on the training set. 
# Then compare its performance with the previous models by calculating the predictions and the RMSE.

library(gbm)
Boston_boost <- gbm(medv ~ ., data = Boston_train, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
Boston_boost_predict <- predict(Boston_boost, Boston_test)
Boston_boost_RMSE = RMSE(expected = Boston_boost_predict,observed = Boston_test$medv)
Boston_boost_RMSE
# We had 5.016083 for regression, 5.051138 for tree 3.889804 for bagging , 4.158363 for random forest and now 3.657824  for boosting.
# So we the best model is with boosting

plot(Boston_boost_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     main = "Predicted vs Actual : Boosted, Test Data")
abline(0,1,col='blue')



#'###  15 Show the summary of the boosted model. A figure of the variable importance will be shown. 

summary(Boston_boost)
# We can notice that lstat and rm still have the more importance in this model, but neither does indus.

#'## Comparison


#'###  16. Reproduce the following comparison: A table in which we show the obtained RMSE with each tested model, you can create a  5×2data.frame 
#in which you put the names of the models and the corresponding RMSE. To visualize the data frame in the compiled html report you can use the
#kable() function from the knitr package. Or, compare the models by plotting the Actual (reality) response values against the predicted values.

library(knitr)

comparison = data.frame(model = c("Regression","Tree","Bagged","Random Forest","Boosted"),
           RMSE = c(Boston_regression_RMSE,Boston_tree_RMSE,Boston_bagged_RMSE,Boston_RandF_RMSE,Boston_boost_RMSE)
           )


kable(comparison)

plot(Boston_tree_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     panel.first = grid(lty=2),
     main = "Predicted vs Actual : Single Tree, Test Data")
abline(0,1,col='blue')

plot(Boston_bagged_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     panel.first = grid(lty=2),
     main = "Predicted vs Actual : Bagging, Test Data")
abline(0,1,col='blue')

plot(Boston_RandF_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     panel.first = grid(lty=2),
     main = "Predicted vs Actual : Random Forest, Test Data")
abline(0,1,col='blue')

plot(Boston_boost_predict,Boston_test$medv,col='red',pch=21,
     xlab = "Predicted",
     ylab = "Actual",
     panel.first = grid(lty=2),
     main = "Predicted vs Actual : Boosted, Test Data")
abline(0,1,col='blue')





#'## Classification Trees


library(rpart)
library(rpart.plot)


spam_dataset = read.csv("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/TD5/spam.csv")
set.seed(18)
spam_idx = sample(1:nrow(spam_dataset), nrow(spam_dataset) / 2)
spam_train = spam_dataset[spam_idx,]
spam_test = spam_dataset[-spam_idx,]

#'### Regression
spam_regression <- glm(spam ~ ., data = spam_train)
spam_regression_predict <- predict(spam_regression, spam_test)

#'###  Single tree
spam_tree <- rpart(spam ~ ., data = spam_train)
spam_tree_predict <- predict(spam_tree, spam_test)
rpart.plot(spam_tree)

#'###  Bagging
p = length(spam_train)-1
spam_bagged <- randomForest(spam ~ ., data = spam_train, mtry=p)
spam_bagged_predict <- predict(spam_bagged, spam_test)

#'###  Random Forest
p = length(spam_train)-1
spam_RF <- randomForest(spam ~ ., data = spam_train, mtry=(p/3))
spam_RF_predict <- predict(spam_RF, spam_test)

#'###  Boosting
spam_boost <- gbm(spam ~ ., data = spam_train, distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
spam_boost_predict <- predict(spam_boost, spam_test)


Performance <- function(expected,observed){
  require(ROCR)
  score <- prediction(expected,observed)
  performance(score,"auc")
  plot(performance(score,"tpr","fpr"),col="green")
  abline(0,1,lty=8)
}

Accuracy <- function(expected,observed){
  clean_expected = ifelse(expected>0.5,TRUE,FALSE)
  # Confusion Matrix
  cm = table(clean_expected,observed)
  print(cm)
  acc = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(acc)
}

#'###  Regression
Performance(spam_regression_predict,spam_test$spam)
Accuracy(spam_regression_predict,spam_test$spam)
#'###  Single tree
Performance(spam_tree_predict,spam_test$spam)
Accuracy(spam_tree_predict,spam_test$spam)
#'###  Bagging
Performance(spam_bagged_predict,spam_test$spam)
Accuracy(spam_bagged_predict,spam_test$spam)
#'###  Random Forest
Performance(spam_RF_predict,spam_test$spam)
Accuracy(spam_RF_predict,spam_test$spam)
#'###  Boosting
Performance(spam_boost_predict,spam_test$spam)
Accuracy(spam_boost_predict,spam_test$spam)

# Conclusion : Boosting has the best accuracy and therefore seems to be the best model,
# even if Random Forest and Bagging are quite good too.
