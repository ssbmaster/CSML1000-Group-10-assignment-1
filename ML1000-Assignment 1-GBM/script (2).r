library(dplyr)
library(gbm)
library(caTools)
library(pROC)

motor_collision_crash_clean_data <- read.csv("C:\\Users\\Alex Fung\\Documents\\CSML1000\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))

data$precinct = as.factor(data$precinct)
data$month = as.ordered(data$month)
data$week = as.ordered(data$week)
data$day = as.ordered(data$day)
data$weekday = as.ordered(data$weekday)
data$hour = as.ordered(data$hour)

data_sample = sample.split(data$did_crash_happen,SplitRatio=0.80)
train_data = subset(data, data_sample==TRUE)
test_data = subset(data, data_sample==FALSE)

model_gbm <- gbm(did_crash_happen ~ ., distribution = "bernoulli", data = rbind(train_data,test_data), n.trees = 50, interaction.depth = 20, n.minobsinnode = 100, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data)), verbose = TRUE)

gbm.iter = gbm.perf(model_gbm, method = "test")

gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)

gbm_auc = roc(test_data$did_crash_happen, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)
