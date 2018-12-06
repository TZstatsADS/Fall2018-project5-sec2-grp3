# Two-Step Models (Merging with Step 1)

library(xgboost)
library(DMwR)
library(earth)
library(zoo)

#Boosted Classification Trees (After SMOTE sampling for imbalance correction) + Xgboost (1) + MARS (2)

load("./data/oldcleaned.Rdata")

#Splitting into training and test sets
set.seed(100)
samp <- sample(nrow(newdat), 0.7*nrow(newdat), replace = FALSE)
train <- newdat[samp,]
test <- newdat[-samp,]

# SMOTE resampled training set
smote_train <- SMOTE(is.paid ~ ., data  = data.frame(train))

# Step 1: Fitting the classfication model
set.seed(100)
smote.xgbtree <- train(factor(is.paid) ~ .,
                       data = smote_train[,-1],
                       tuneGrid = grid_default,
                       method = "xgbTree",
                       trControl = trainControl(method = "cv"))

# Predicting the class labels for the test set
smote.xgb.pred <- predict(smote.xgbtree, newdata = test[,-1])

# Create a new set for the regression
newset <- cbind(test,smote.xgb.pred)
newset$id <- index(newset)

# Only care about the paid classified users
paidset <- newset[newset$smote.xgb.pred == 1,][,-c(2,16)]
freeset <- newset[newset$smote.xgb.pred == 0,][,-c(2,16)]

#Step 2: Fitting the regression models

#Splitting into training & Test again
set.seed(100)
samp <- sample(nrow(paidset), 0.7*nrow(paidset), replace = FALSE)
training <- paidset[samp,]
valid <- paidset[-samp,]
summary(training)
summary(valid)

# Create a model matrix
mod <- model.matrix(revenue ~ ., data = training[,-17])
modte <- model.matrix(revenue ~ ., data = valid[,-17])

# The Step 2 (Model 1 - XgBoost)
xgb.final <- xgboost(data = mod, 
               label = training$revenue, 
               eta = 0.01,
               max_depth = 4, 
               nrounds = 1000,
               eval_metric = "rmse",
               objective = "reg:linear"#,
               #tweedie_variance_power = 1.1
)

#Predicted revenue 
y_pred.xgb <- predict(xgb.final, newdata = modte)

#RMSE
sqrt(mean((y_pred.xgb-valid$revenue)^2))



# Model 2 (Multivariate Adaptive Regression Splines)
mars <- earth(revenue~., data = training, degree = 3)

y_pred.mars <- predict(mars, newdata = valid)

sqrt(mean((y_pred.mars-valid$revenue)^2))


#We need to calculate the final mse which includes the following
finalpred <- rbind(training, valid, freeset)
sqrt(mean((test[-training$id,]$revenue-finalpred[-training$id,]$revenue)^2))
# vectors of final predictions
# this is a bit tricky because we actually have three groups:
# 1. points classified as "unpaid"
# 2. points classified as "paid" but used in the training set
# 3. points classified as "paid" but used in the test set
# we will first create a vector that indicates which of the above groups each point is in
# they are numbered in the order they are listed above
