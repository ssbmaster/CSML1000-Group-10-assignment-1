library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(doParallel)
library(caret)

registerDoParallel(cores = 2)

motor_collision_crash_clean_data <- read.csv("C:\\Users\\Alex Fung\\Documents\\CSML1000\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, total_number_of_crashes))

#set all columns as categorical factors (aside from timestamp)
data$precinct = as.factor(data$precinct)
data$month = as.factor(data$month)
data$week = as.factor(data$week)
data$day = as.factor(data$day)
data$weekday = as.factor(data$weekday)
data$hour = as.factor(data$hour)
data$did_crash_happen = as.factor(data$did_crash_happen)


#rather than do random split, we should use time series split
data_sample = sample.split(data$did_crash_happen,SplitRatio=0.80)
train_data = subset(data, data_sample==TRUE)
test_data = subset(data, data_sample==FALSE)

#### creating sampling seeds ####
set.seed(123)
seeds <- vector(mode = "list", length = 432)
for(i in 1:431) seeds[[i]] <- sample.int(1000, 5)

## For the last model:
seeds[[432]] <- sample.int(1000, 1)

#time series split
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              verboseIter = TRUE
                              )

#5 different combinations of tuning parameters for each model
tuneLength.num <- 5

#caret train
model_gbm.mod <- train(did_crash_happen ~ . - timestamp, 
                       data = data, 
                       method = "gbm",
                       family="gaussian",
                       trControl = myTimeControl, 
                       tuneLength = tuneLength.num
                       )


#gbm
model_gbm <- gbm(did_crash_happen ~ . - timestamp, distribution = "bernoulli", data = rbind(train_data,test_data), n.trees = 50, interaction.depth = 20, n.minobsinnode = 100, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data)), verbose = TRUE)

gbm.iter = gbm.perf(model_gbm, method = "test")

gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)

gbm_auc = roc(test_data$did_crash_happen, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)
