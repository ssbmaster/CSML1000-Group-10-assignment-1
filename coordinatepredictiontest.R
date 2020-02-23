getwd()
crashes_data <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))


library(dplyr)
library(sp)

# Get rid of data that has NA's in Longitude; should correspond with Latitude NA's so good enough
crashes_data_clean <- crashes_data[!(is.na(crashes_data$LONGITUDE)),]

# Drop 3rd,4th and 5th columns of the dataframe
crashes_data_clean <- crashes_data_clean[c(1, 2, 3, 5, 6)]

# Change the dates to dates datatype and use only the hours for prediction for now
crashes_data_clean$CRASH.DATE <- as.Date(crashes_data_clean$CRASH.DATE, format='%m/%d')
crashes_data_clean$CRASH.TIME <- as.numeric(gsub("[:punct:][0-9]{2}$", "", crashes_data_clean$CRASH.TIME))

# Round the digits in Lat and Long so that there are fewer distinct "points"
crashes_data_clean$LONGITUDE <- round(crashes_data_clean$LONGITUDE, digits=3)
crashes_data_clean$LATITUDE <- round(crashes_data_clean$LATITUDE, digits=3)

# Change regular dataframe to a Spatial Points Dataframe using columsn labeled "LONGITUDE" and "LATITUDE"
coordinates(crashes_data_clean) <- ~LONGITUDE+LATITUDE

summary(crashes_data_clean)
summary(crashes_data_clean@coords) # How to view the coordinates

library(caTools)
set.seed(123)
data_sample = sample.split(crashes_data_clean$CRASH.DATE,SplitRatio=0.80)
train_data = subset(crashes_data_clean,data_sample==TRUE)
test_data = subset(crashes_data_clean,data_sample==FALSE)
dim(train_data)
dim(test_data)


#DECISION TREE ALGO

# Next, we will implement a decision tree algorithm. Decision Trees to plot the outcomes of a decision. These outcomes are basically a consequence through
# which we can conclude as to what class the object belongs to. We will now implement our decision tree model and will plot it using the rpart.plot()
# function. We will specifically use the recursive parting to plot the decision tree.

library(rpart)
library(rpart.plot)
library(pROC)
decisionTree_model <- rpart(CRASH.TIME ~ . , train_data, method = 'class')
predicted_val <- predict(decisionTree_model, test_data, type = 'class')
probability <- predict(decisionTree_model, test_data, type = 'prob')
rpart.plot(decisionTree_model, box.palette = 'Blues')
auc.rp = roc(test_data$CRASH.TIME, factor(predicted_val, ordered = TRUE), plot = TRUE, col = "blue")
auc.rp

library(neuralnet)
ANN_model =neuralnet (CRASH.TIME~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)
auc.nn = roc(test_data$CRASH.TIME, factor(resultANN, ordered = TRUE), plot = TRUE, col = "blue")
auc.nn

library(gbm, quietly=TRUE)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(CRASH.TIME ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")

model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
#Plot the gbm model
plot(model_gbm)

# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$CRASH.TIME, gbm_test, plot = TRUE, col = "red")
print(gbm_auc)
gbm_auc
