crashes_data <- read.csv('../crashes.csv')

crashes_data_clean <- crashes_data[!(is.na(crashes_data$ZIP.CODE)),]

library(dplyr)


# Drop 3rd,4th and 5th columns of the dataframe
crashes_data_clean <- select(crashes_data_clean,-c(3,5:29))

summary(crashes_data_clean$ZIP.CODE)

library(caTools)
set.seed(123)
data_sample = sample.split(crashes_data_clean$ZIP.CODE,SplitRatio=0.80)
train_data = subset(crashes_data_clean,data_sample==TRUE)
test_data = subset(crashes_data_clean,data_sample==FALSE)
dim(train_data)
dim(test_data)

# Num of zip codes in clean data
as.data.frame(table(crashes_data_clean$ZIP.CODE))

#DECISION TREE ALGO

# Next, we will implement a decision tree algorithm. Decision Trees to plot the outcomes of a decision. These outcomes are basically a consequence through
# which we can conclude as to what class the object belongs to. We will now implement our decision tree model and will plot it using the rpart.plot()
# function. We will specifically use the recursive parting to plot the decision tree.

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(ZIP.CODE ~ . , train_data, method = 'class')
predicted_val <- predict(decisionTree_model, test_data, type = 'class')
probability <- predict(decisionTree_model, test_data, type = 'prob')
rpart.plot(decisionTree_model)
auc.rp = roc(test_data$Class, factor(predicted_val, ordered = TRUE), plot = TRUE, col = "blue")
auc.rp
