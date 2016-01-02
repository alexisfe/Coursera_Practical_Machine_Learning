library(caret)
library(RANN)

set.seed(1234)

data_csv_link <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_csv_link <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

dep_var = "classe"

data <- read.csv(data_csv_link)
eval_data <- read.csv(test_csv_link)

#Identify and remove poor predictors using near zero variance
good_preds <- row.names(subset(nearZeroVar(data, saveMetrics = TRUE), nzv == FALSE))

data <- data[, good_preds]

eval_data <- eval_data[, good_preds[good_preds != dep_var]]

inTrain <- createDataPartition(data[, dep_var], p = 3/4)[[1]]
train <- data[inTrain,]
test <- data[-inTrain,]

train_pred <- train[, -which(names(train) == dep_var)]
test_pred <- test[, -which(names(test) == dep_var)]

train.ctrl <- trainControl(method="cv", number=5) #Cross-validation using 5 folds

train_pca <- predict(preProcess(train_pred, method="pca", thresh=0.95), train_pred)

#Preprocess data to impute NAs
preproc <- preProcess(train_pred, method="knnImpute")

train_pred <- predict(preproc, train_pred)
test_pred <- predict(preproc, test_pred)

model_pca <- train(train[, dep_var] ~., method="rf", trControl=train.ctrl, data=train_pca)
model <- train(train[, dep_var] ~., method="rf", trControl=train.ctrl, data=train_pred)

test_pca <- predict(preproc, test_pred)

cm_pca <- confusionMatrix(test[, dep_var], predict(model_pca, test_pca))

cm <- confusionMatrix(test[, dep_var], predict(model, test_pred))

cm_pca$overall["Accuracy"]
cm$overall["Accuracy"]