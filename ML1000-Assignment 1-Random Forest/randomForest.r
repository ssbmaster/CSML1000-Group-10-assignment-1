library(dplyr)
library(caTools)
library(pROC)
library(doParallel)
library(caret)
#library(DMwR)
#library(ROSE)
library(MLmetrics)
library(randomForest)

#setup cluster (forget about this lmao)
#cl <- makePSOCKcluster(2) #4 clusters on my machine
#registerDoParallel(cl)


motor_collision_crash_clean_data <- read.csv("C:\\Users\\alexf\\Documents\\CSML1000\\Assignments\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, total_number_of_crashes))

data$timestamp = as.Date(data$timestamp)
data$precinct = as.character(data$precinct)
data$month = as.numeric(data$month)
data$week = as.numeric(data$week)
data$day = as.numeric(data$day)
data$weekday = as.numeric(data$weekday)
data$hour = as.numeric(data$hour)
data$did_crash_happen = as.factor(
  ifelse(data$did_crash_happen ==  0, "no", "yes")
)

set.seed(123)
data_newer = data[data$timestamp > '2018-01-27', ]
data_sample = sample.split(data_newer$hour,SplitRatio=0.80)
trainData = subset(data_newer, data_sample==TRUE)
testData = subset(data_newer, data_sample==FALSE)

trainData <- select(trainData, -c(timestamp))
testData <- select(testData, -c (timestamp))

set.seed(123)
columns = colnames(trainData)
trainData_upsampled = upSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_upsampled$did_crash_happen))

#try downsampling instead...
trainData_downsampled = downSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_downsampled$did_crash_happen))

#remove unnecessary data objects
rm(motor_collision_crash_clean_data, data, data_newer, data_sample, trainData)
rm(trainData_upsampled)
gc()

set.seed(123)
ptm_rf <- proc.time()
#random forest @ 500 trees, mtry = 2 
model_rf = randomForest(
  did_crash_happen ~ .,
  data = trainData_downsampled,
  importance = TRUE,
  do.trace=10,
  ntree = 250,
  mtry = 2
)
proc.time() - ptm_rf


pred.model_rf.prob = predict(model_rf, newdata = testData, type="prob")
pred.model_rf.raw = predict(model_rf, newdata = testData)

roc.model_rf = pROC::roc(
  testData$did_crash_happen, 
  as.vector(ifelse(pred.model_rf.prob[,"yes"] >0.5, 1,0))
)
auc.model_rf = pROC::auc(roc.model_rf)
print(auc.model_rf)

plot.roc(roc.model_rf, print.auc = TRUE, col = 'red' , print.thres = "best" )

confusionMatrix(data = pred.model_rf.raw, testData$did_crash_happen)

#summary of model 
summary(model_rf)

#see the different metrics and roc curve this model scored against trainData_downsampled
pred.model_rf.train.prob = predict(model_rf, newdata = trainData_downsampled, type="prob")
pred.model_rf.train.raw = predict(model_rf, newdata = trainData_downsampled)

roc.model_rf.train = pROC::roc(
  trainData_downsampled$did_crash_happen, 
  as.vector(ifelse(pred.model_rf.train.prob[,"yes"] >0.5, 1,0))
)
auc.model_rf.train = pROC::auc(roc.model_rf.train)
print(auc.model_rf.train)

#plot ROC curve
plot.roc(roc.model_rf.train, print.auc = TRUE, col = 'blue' , print.thres = "best" )

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_rf.train.raw, trainData_downsampled$did_crash_happen)
