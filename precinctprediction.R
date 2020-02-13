crashdata <- read.csv("../data/motor_vehicle_collisions_crashes_cleaned.csv", header = TRUE, na.strings = c("NA","","#NA"))

crashdata <- crashdata[c('precinct', 'month', 'week', 'day', 'weekday', 'hour', 'did_crash_happen')]

crashdata$precinct <- as.character(crashdata$precinct)

library(caTools)
set.seed(123)
data_sample = sample.split(crashdata$hour, SplitRatio=0.80)
train_data = subset(crashdata,data_sample==TRUE)
test_data = subset(crashdata,data_sample==FALSE)
dim(train_data)
dim(test_data)

library(rpart)
library(rpart.plot)
library(pROC)

fit <- rpart(did_crash_happen ~ ., train_data, method="anova")
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
prune(fit, cp=0.01)
predicted_val <- predict(fit, test_data)
#probability <- predict(fit, test_data, type = 'prob')

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
rpart.plot(fit, main="Regression Tree", box.palette = "Blues", under=TRUE, roundint = FALSE, clip.right.labs = FALSE, type=3, split.fun=splitfun)
rpart.rules(fit, cover=TRUE, roundint = FALSE)
auc.rp = roc(test_data$did_crash_happen, factor(predicted_val, ordered = TRUE), plot = TRUE, col = "blue")
#post(fit, file = "tree.ps", title = "Pruned Regression Tree for Crashes")
auc(test_data$did_crash_happen, predicted_val)
auc.rp

# Save the model into a file
save(fit, file="fittedRegTreeModel.rda")
