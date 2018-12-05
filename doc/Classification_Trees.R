# Different Classfication Tree Models to run for Step 1

# 1) Classfication Tree

library(caret)
library(tidyverse)
library(zoo)
library(DMwR)
library(rpart)

# Splitting the data into training and test
set.seed(100)
samp <- sample(nrow(dat), 0.7*nrow(dat), replace = FALSE)
train <- dat[samp,]
test <- dat[-samp,]

# Fitting the model
set.seed(100)
cltree <- train(is.paid ~ .,
                data = train[,-1],
                parms = list(split = "gini"),
                method = "rpart",
                trControl = trainControl(method = "cv"))


# Predicting the class labels for the test set
ct.pred <- predict(cltree, newdata = test[,-1])

# Confusion Matrix for the predicted classes (positive class is is.paid = 1)
conf <- table(ct.pred, test$is.paid)

# Precision (true pos/true pos + false pos)
conf[2,2]/sum(conf[2,])

# Recall (true pos/true pos + false neg)
conf[2,2]/sum(conf[,2])


# 2) ROSEd classification tree

#Final ROSEd dataset
rose_train <- ROSE(is.paid ~ ., data  = data.frame(train))$data

# Fitting the model
set.seed(100)
rose.cltree <- train(factor(is.paid) ~ .,
                data = rose_train[,-1],
                parms = list(split = "gini"),
                method = "rpart",
                trControl = trainControl(method = "cv"))


# Predicting the class labels for the test set
rose.ct.pred <- predict(rose.cltree, newdata = test[,-1])

# Confusion Matrix for the predicted classes (positive class is is.paid = 1)
conf <- table(rose.ct.pred, test$is.paid)

# Precision (true pos/true pos + false pos)
conf[2,2]/sum(conf[2,])

# Recall (true pos/true pos + false neg)
conf[2,2]/sum(conf[,2])


