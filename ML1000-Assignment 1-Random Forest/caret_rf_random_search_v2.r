#install.packages('dplyr')
#install.packages('gbm')
#install.packages('caTools')
#install.packages('pROC')
#install.packages('doParallel')
#install.packages('caret')
#install.packages('e1071', dependencies=TRUE)
#install.packages('DMwR')
#install.packages('ROSE')

library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(doParallel)
library(caret)
#library(DMwR)
#library(ROSE)
library(MLmetrics)

#setup cluster (forget about this lmao)
#cl <- makePSOCKcluster(2) #4 clusters on my machine
#registerDoParallel(cl)


motor_collision_crash_clean_data <- read.csv("C:\\Users\\alexf\\Documents\\CSML1000\\Assignments\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, total_number_of_crashes))

data$timestamp = as.Date(data$timestamp)
data$precinct = as.factor(data$precinct)
data$month = as.numeric(data$month)
data$week = as.numeric(data$week)
data$day = as.numeric(data$day)
data$weekday = as.numeric(data$weekday)
data$hour = as.numeric(data$hour)
data$did_crash_happen = as.factor(
  ifelse(data$did_crash_happen ==  0, "no", "yes")
)

#time series split
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 674520,
#                               horizon = 674520,
#                               fixedWindow =  TRUE,
#                               #allowParallel = TRUE,
#                               verboseIter = TRUE#,
#                               #sampling = "smote"
#                               )

#create time slices
# timeSlices <- createTimeSlices(1:nrow(data),
#                                initialWindow = 36, horizon = 12, fixedWindow = TRUE)
# trainSlices <- timeSlices[[1]]
# testSlices <- timeSlices[[2]]

#separate the data into train and test
#because we run into memory issues 
#train_data will be the data from 2017-01-
#test_data will be from 2019-01-26 till 2020-01-26
trainData_older = data[data$timestamp < '2017-01-27', ]
trainData = data[data$timestamp > '2017-01-27' & data$timestamp < '2019-01-27', ]
testData = data[data$timestamp > '2019-01-26', ]

#splitting by timestamp
# splitIdx = createDataPartition(data$did_crash_happen, p=0.7, list = FALSE)  # 70% training data, 30% testing
# trainData = data[splitIdx, ]
# testData = data[-splitIdx, ]

#we will need to upsample the trainData
set.seed(123)
columns = colnames(trainData)
trainData_upsampled = upSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_upsampled$did_crash_happen))

set.seed(456)
#try downsampling instead...
trainData_downsampled = downSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_downsampled$did_crash_happen))


rf.trainControl = trainControl(
  method = "cv", 
  number = 5, # it takes forever for 10 - fold 
  # Estimate class probabilities
  classProbs = TRUE,
  # Evaluate performance using the following function
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verbose = TRUE
)

#make sure we get rid as much useless data objects in R environment as possible
gc()
rm(motor_collision_crash_clean_data, data, trainData, trainData_older)
rm(trainData_upsampled)
#rm(trainData_downsampled)
gc()
memory.limit()
memory.limit(size=30000)

#train model
set.seed(789)
ptm_rf <- proc.time()
model_gbm <- train(
  did_crash_happen ~ . - timestamp,
  #data = data[trainSlices[[1]],],
  data = trainData_downsampled,
  #data = train_data,
  method = "rf",
  #family="gaussian",
  #distribution = "gaussian",
  trControl = rf.trainControl,
  tuneLength = 7
  #tuneGrid = gbmGrid
)
proc.time() - ptm_rf

#when we are done with parallel processing needs
#stopCluster(cl)

#make predictions aginst testData with the new model 
print(model_gbm)
pred.model_gbm.prob = predict(model_gbm, newdata = testData, type="prob")
pred.model_gbm.raw = predict(model_gbm, newdata = testData)


roc.model_gbm = pROC::roc(
  testData$did_crash_happen, 
  as.vector(ifelse(pred.model_gbm.prob[,"yes"] >0.5, 1,0))
)
auc.model_gbm = pROC::auc(roc.model_gbm)
print(auc.model_gbm)

#plot ROC curve
plot.roc(roc.model_gbm, print.auc = TRUE, col = 'red' , print.thres = "best" )

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_gbm.raw, testData$did_crash_happen)

#summary of model 
summary(model_gbm)

d