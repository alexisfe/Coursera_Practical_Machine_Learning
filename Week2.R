#Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

#Question 2

library(ggplot2)
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

ggplot(testing, aes(Superplasticizer)) + geom_histogram() + scale_x_log10()

#Question 3

library(ggplot2)
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
il_predictors = predictors[,grep("^IL", names(predictors))]
adData = data.frame(diagnosis,il_predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

training_pred <- training[, -which(names(training) == "diagnosis")]
testing_pred <- testing[, -which(names(testing) == "diagnosis")]

#Exploratory analysis
training_pred.melt <- melt(training_pred)

ggplot(training_pred.melt, aes(value)) +
  geom_histogram() +
  facet_grid(variable ~ .)

#Prediction
preproc <- preProcess(training_pred, method="pca", thresh=0.9)
preproc$numComp

train <- predict(preproc, training_pred)

model <- train(training$diagnosis ~., method="glm", data=train)

test <- predict(preproc, testing_pred)

confusionMatrix(testing$diagnosis, predict(model, test))

#Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
il_predictors = predictors[,grep("^IL", names(predictors))]
adData = data.frame(diagnosis,il_predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

training_pred <- training[, -which(names(training) == "diagnosis")]
testing_pred <- testing[, -which(names(testing) == "diagnosis")]

#Prediction
preproc <- preProcess(training_pred, method="pca", thresh=0.8)

train_pca <- predict(preproc, training_pred)

model_pca <- train(training$diagnosis ~., method="glm", data=train_pca)
model <- train(training$diagnosis ~., method="glm", data=training_pred)

test_pca <- predict(preproc, testing_pred)

cm_pca <- confusionMatrix(testing$diagnosis, predict(model_pca, test_pca))

cm <- confusionMatrix(testing$diagnosis, predict(model, testing_pred))

cm_pca$overall["Accuracy"]
cm$overall["Accuracy"]
