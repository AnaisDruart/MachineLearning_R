library(caTools)

dataset = read.csv("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/Hackaton/data.csv")
testing_dataset = read.csv("C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/Hackaton/test.csv")

str(dataset)
dataset = dataset[1:14]
testing_dataset = testing_dataset[1:13]
dataset = scale(dataset, scale=TRUE)
testing_dataset = scale(testing_dataset, scale=TRUE)
#logistic regression

summary(dataset)
#BACKWARD
summary(aov(target ~ ., data=dataset))
summary(aov(target ~ (.-time_signature), data=dataset))
summary(aov(target ~ (.-time_signature - key), data=dataset))
summary(aov(target ~ (.-time_signature - key - artist), data=dataset))
summary(aov(target ~ (.-time_signature - key - artist - liveness), data=dataset))
summary(aov(target ~ (.-time_signature - key - artist - liveness - energy - song_title), data=dataset))
summary(aov(target ~ (.-time_signature - key - artist - liveness - energy - song_title - mode), data=dataset))
summary(aov(target ~ (.-time_signature - key - artist - liveness - energy - song_title - mode -tempo), data=dataset))

classifier.logreg <- glm(target ~acousticness + danceability + duration_ms + instrumentalness + loudness + speechiness + valence , family = binomial,data = Train_train)
classifier.logreg

summary(classifier.logreg)

pred.glm = predict(classifier.logreg, newdata = train_test, type="response")
pred.glm <- ifelse(pred.glm>0.5,1,0)
pred.glm
result = data.frame(cbind(id=1:200,target=pred.glm))

write.csv(result, file="C:/Users/anais/Documents/Esilv/S7/Machine Learning/TD/Hackaton/pred3.csv", row.names = FALSE)


library(ROCR)
sampling=sample(1:nrow(training_set),1500)
Train_train=dataset[sampling,]
train_test=dataset[-sampling,]

pred_model2 = prediction(pred.glm,train_test$target)
AUC_2 = performance(pred_model2,"tpr","fpr")
plot(AUC_2,col="coral")
abline(0,1,col = "lightgrey")