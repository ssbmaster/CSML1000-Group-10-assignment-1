install.packages('dplyr')
install.packages('gbm')
install.packages('caTools')
install.packages('pROC')
install.packages('doParallel')
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
install.packages('DMwR')
install.packages('ROSE')

library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(doParallel)
library(caret)
library(DMwR)
library(ROSE)


#registerDoParallel(cores = 4)

motor_collision_crash_clean_data <- read.csv("C:\\Users\\alexf\\Documents\\CSML1000\\Assignments\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))

#set all columns as categorical factors (aside from timestamp)
data$precinct = as.factor(data$precinct)
#data$month = as.factor(data$month)
#data$week = as.factor(data$week)
#data$day = as.factor(data$day)
#data$weekday = as.factor(data$weekday)
#data$hour = as.factor(data$hour)
#data$did_crash_happen = as.factor(data$did_crash_happen)


#rather than do random split, we should use time series split
data_sample = sample.split(data$did_crash_happen,SplitRatio=0.80)
train_data = subset(data, data_sample==TRUE)
test_data = subset(data, data_sample==FALSE)

#time series split
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 4,
#                               horizon = 2,
#                               fixedWindow = FALSE,
#                               #allowParallel = TRUE,
#                               verboseIter = TRUE,
#                               sampling = "smote"
#                               )

#5 different combinations of tuning parameters for each model
# tuneLength.num <- 5

#caret train
# model_gbm <- train(did_crash_happen ~ .,# - timestamp,
#                        data = data,
#                        method = "gbm",
#                        #family="gaussian",
#                        #distribution = "gaussian",
#                        trControl = myTimeControl,
#                        tuneLength = 1
#                        )

#create time slices
# timeSlices <- createTimeSlices(1:nrow(data), 
#                                initialWindow = 36, horizon = 12, fixedWindow = TRUE)
# trainSlices <- timeSlices[[1]]
# testSlices <- timeSlices[[2]]


#
# for(i in 1:length(trainSlices)){
#   print(i)
#   rfModel <- train(
#     did_crash_happen ~ precinct + month + week + day + weekday + hour, #- timestamp,
#     data = data[trainSlices[[i]],],
#     method = "rf", 
#   )
#   # pred <- predict(gbmModel, data[testSlices[[i]],])
#   # 
#   # 
#   # true <- data$did_crash_happen[testSlices[[i]]]
#   # plot(true, col = "red", ylab = "true (red) , pred (blue)", 
#   #      main = i, ylim = range(c(pred,true)))
#   # points(pred, col = "blue") 
# }


#gbm
model_gbm <- gbm(did_crash_happen ~., data = rbind(train_data,test_data), distribution='bernoulli', n.trees=500, interaction.depth=3, n.minobsinnode=100, shrinkage=0.01, bag.fraction = 0.5, train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data)), verbose = TRUE)

gbm.iter = gbm.perf(model_gbm, method = "test")

gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)

gbm_auc = roc(test_data$did_crash_happen, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)
