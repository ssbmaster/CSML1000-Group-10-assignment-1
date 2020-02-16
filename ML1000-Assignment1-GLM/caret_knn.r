library(dplyr)
library(caTools)
library(pROC)
library(caret)
library(e1071)

motor_collision_crash_clean_data <- read.csv("C:\\Users\\Alex Fung\\Documents\\CSML1000\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, total_number_of_crashes))

#set columns as appropriate
data$timestamp = as.Date(data$timestamp)
data$precinct = as.factor(data$precinct)
data$month = as.ordered(data$month)
data$week = as.ordered(data$week)
data$day = as.ordered(data$day)
data$weekday = as.ordered(data$weekday)
data$hour = as.ordered(data$hour)
data$did_crash_happen = as.factor(
  ifelse(data$did_crash_happen == 0, "no", "yes")
)

#separate data into train and test
#because we run into memory issues
#trainData will be two years' worth, from 2017 Jan to 2019 Jan
#testData will be from 2019 Jan onwards
#a separate 'trainData_older' will be all the data from 2017 Jan and before
trainData_older = data[data$timestamp < '2017-01-27', ]
trainData = data[data$timestamp > '2017-01-27' & data$timestamp < '2019-01-27', ]
testData = data[data$timestamp > '2019-01-27', ]

#we will need to downsample the trainData
set.seed(123)
columns = colnames(trainData)
trainData_downsampled = downSample(
  x = trainData[, columns[columns != 'did_crash_happen']],
  y = trainData$did_crash_happen, list = FALSE, yname = 'did_crash_happen'
)
print(table(trainData_downsampled$did_crash_happen))

knn.trainControl = trainControl(
  method = "cv", 
  number = 3, 
  #Estimate class probabilities
  classProbs = TRUE,
  #Evaluate performance using the following function
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verbose = TRUE
)

#make sure we get rid as much useless data objects in R environment as possible
gc()
rm(motor_collision_crash_clean_data, data, trainData)
gc()
memory.limit()
memory.limit(size = 16000)

#no need to tuneGrid because logistic regression using glm has no parameters
#train model
set.seed(789)
ptm_rf <- proc.time()
model_knn <- train(
  did_crash_happen ~ . - timestamp, 
  data = trainData_downsampled,
  method = 'glm',
  trControl = knn.trainControl,
  tuneLength = 7
)
proc.time() - ptm_rf

#make prediction against testData with the new model
print(model_glm)
pred.model_glm.prob = predict(model_glm, newdata = testData, type="prob")
pred.model_glm.raw = predict(model_glm, newdata = testData)

roc.model_glm = pROC::roc(
  testData$did_crash_happen,
  as.vector(ifelse(pred.model_glm.prob[,"yes"] > 0.5, 1, 0))
)
auc.model_glm = pROC::auc(roc.model_glm)
print(auc.model_glm)

#plot ROC curve
plot.roc(roc.model_glm, print.auc = TRUE, col = 'red', print.thres = "best")

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_glm.raw, testData$did_crash_happen)

#summary of model
summary(model_glm)