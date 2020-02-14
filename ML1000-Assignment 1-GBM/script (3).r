library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(caret)
library(DMwR)
library(ROSE)


motor_collision_crash_clean_data <- read.csv("C:\\Users\\alexf\\Documents\\CSML1000\\Assignments\\CSML1000-Group-10-assignment-1\\data\\motor_vehicle_collisions_crashes_cleaned.csv")

#data <- select(motor_collision_crash_clean_data, -c(id, timestamp, total_number_of_crashes))
data <- select(motor_collision_crash_clean_data, -c(id, total_number_of_crashes))

data$timestamp = as.Date(data$timestamp)
data$precinct = as.factor(data$precinct)
data$month = as.ordered(data$month)
data$week = as.ordered(data$week)
data$day = as.ordered(data$day)
data$weekday = as.ordered(data$weekday)
data$hour = as.ordered(data$hour)
#data$did_crash_happen = as.factor(data$did_crash_happen) #doesn't work with gbm for some goddamn reason

train_data = data[data$timestamp < '2019-01-27', ]
test_data = data[data$timestamp > '2019-01-26', ]


model_gbm <- gbm(
  did_crash_happen ~ . - timestamp, 
  distribution = "bernoulli", 
  data = train_data,
  n.trees = 100,
  interaction.depth = 10, 
  #n.minobsinnode = 100,
  shrinkage = 0.1,
  #cv.folds = 3,
  #bag.fraction = 0.5, 
  #train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data)),
  verbose = TRUE)


# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)
