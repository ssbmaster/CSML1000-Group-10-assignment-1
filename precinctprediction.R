library(rpart)
library(rpart.plot)
library(pROC)
library(caTools)

crashData <- read.csv("../data/motor_vehicle_collisions_crashes_cleaned.csv", header = TRUE, na.strings = c("NA","","#NA"))

crashData <- crashData[c('precinct', 'month', 'week', 'day', 'weekday', 'hour', 'did_crash_happen')]

# Set precincts to character
crashData$precinct <- as.character(crashData$precinct)

# Partition the data into test and train data sets
set.seed(123)
dataSample = sample.split(crashData$hour, SplitRatio=0.80)
trainData = subset(crashData, dataSample==TRUE)
testData = subset(crashData, dataSample==FALSE)
dim(trainData)
dim(testData)

# Clean up some data to free up memory
rm(crashData)

# Further isolate the train and test data sets to x (input) and y (target/output) for less confusion
xTrain <- trainData[c("precinct", "month", "week", "day", "weekday", "hour")]
yTrain <- as.data.frame(trainData[c("did_crash_happen")])
xTest <- testData[c("precinct", "month", "week", "day", "weekday", "hour")]

# Determine the model by running regression tree algo on it
regTreeModel <- rpart(did_crash_happen ~ ., trainData, method="anova")

# Find the min of x-error to determine the cp and prune the tree
regTreeModel$cptable[which.min(regTreeModel$cptable[,"xerror"]),"CP"]
prune(regTreeModel, cp=0.01)

# Get the predicted data
predictedData <- predict(regTreeModel, xTest)

# Procedure to limit plot text output to a certain width and wordwrap
splitfun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 40), collapse = "\n")
  }
  labs
}

# Generate the plots
rpart.plot(regTreeModel, main="Regression Tree", box.palette = "Blues", under=TRUE, roundint = FALSE, clip.right.labs = FALSE, type=3, split.fun=splitfun)
rpart.rules(regTreeModel, cover=TRUE, roundint = FALSE)
auc.rp = roc(testData$did_crash_happen, factor(predictedData, ordered = TRUE), plot = TRUE, col = "blue")
auc(testData$did_crash_happen, predictedData)
auc.rp

# TODO: This doesn't work !!! the predictedData is a a vector of probabilities it seems, not just 1 or 0.
library(caret)
confusionMatrix(predictedData, testData$did_crash_happen)

# Save the model into a file
save(regTreeModel, file="fittedRegTreeModel.rda")
