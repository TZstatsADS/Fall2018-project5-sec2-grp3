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
conf <- table(ct.pred, test$is.paid); conf

# Precision (true pos/true pos + false pos)
prec <- conf[2,2]/sum(conf[2,]); prec

# Recall (true pos/true pos + false neg)
rec <- conf[2,2]/sum(conf[,2]); rec

# F1 Score
f1 <- 2*(rec*prec)/(rec+prec); f1


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
rose.conf <- table(rose.ct.pred, test$is.paid); rose.conf

# Precision (true pos/true pos + false pos)
rose.prec <- rose.conf[2,2]/sum(rose.conf[2,]); rose.prec

# Recall (true pos/true pos + false neg)
rose.rec <- rose.conf[2,2]/sum(rose.conf[,2]); rose.rec

# F1 Score
rose.f1 <- 2*(rose.rec*rose.prec)/(rose.rec+rose.prec); rose.f1



# 3) SMOTEd classification tree

#Final SMOTEd dataset
smote_train <- SMOTE(is.paid ~ ., data  = data.frame(train))

# Fitting the model
set.seed(100)
smote.cltree <- train(factor(is.paid) ~ .,
                     data = smote_train[,-1],
                     parms = list(split = "gini"),
                     method = "rpart",
                     trControl = trainControl(method = "cv"))


# Predicting the class labels for the test set
smote.ct.pred <- predict(smote.cltree, newdata = test[,-1])

# Confusion Matrix for the predicted classes (positive class is is.paid = 1)
smote.conf <- table(smote.ct.pred, test$is.paid); smote.conf

# Precision (true pos/true pos + false pos)
smote.prec <- smote.conf[2,2]/sum(smote.conf[2,]); smote.prec

# Recall (true pos/true pos + false neg)
smote.rec <- smote.conf[2,2]/sum(smote.conf[,2]); smote.rec

# F1 Score
smote.f1 <- 2*(smote.rec*smote.prec)/(smote.rec+smote.prec); smote.f1


# Summary

eval.sum <- data.frame(method = c("Normal","ROSE","SMOTE"),
                       precision = c(prec,rose.prec,smote.prec),
                       recall = c(rec,rose.rec,smote.rec),
                       f1score = c(f1,rose.f1,smote.f1))

save(eval.sum, file = "./data/cltree-eval.Rdata")


