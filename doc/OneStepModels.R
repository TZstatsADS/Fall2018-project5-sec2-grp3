## This is for the imbalanced regression problem. "One Step Model"

library(caret)
library(xgboost)

# XgBoost (Linear Regression followed by Tweedie Regression)
load("./output/cleaned.Rdata")

# Splitting the data into training and test
set.seed(100)
samp <- sample(nrow(dat), 0.7*nrow(dat), replace = FALSE)
train <- dat[samp,]
test <- dat[-samp,]
summary(train)
summary(test)

# Model 1: Xgboost (Linear Objective function)

mod <- model.matrix(revenue ~ ., data = train)
modte <- model.matrix(revenue ~ ., data = test)

# Parameters chosen after cross validation and hyperparameter tuning
xgb <- xgboost(data = mod, 
               label = train$revenue, 
               eta = 0.1,
               max_depth = 8, 
               nrounds = 100,
               eval_metric = "rmse",
               objective = "reg:linear"
)

# Predicting for the test set
y_pred <- predict(xgb, newdata = modte)

# RMSE
xgb.rmse <- sqrt(mean((y_pred-test$revenue)^2)); xgb.rmse
#1.70

# Model 2: Xgboost (Tweedie Distribution Objective function)

# Parameters chosen after cross validation and hyperparameter tuning (eta, tweedie_variance_power, nrounds)
xgb.tweedie <- xgboost(data = mod, 
               label = train$revenue, 
               eta = 0.1,
               max_depth = 8, 
               nrounds = 100,
               eval_metric = "rmse",
               objective = "reg:tweedie",
               tweedie_variance_power = 1.1
)

# Predicting for the test set
y_pred.tweedie <- predict(xgb.tweedie, newdata = modte)

# RMSE
tweedie.rmse <- sqrt(mean((y_pred.tweedie-test$revenue)^2)); tweedie.rmse
#1.68

