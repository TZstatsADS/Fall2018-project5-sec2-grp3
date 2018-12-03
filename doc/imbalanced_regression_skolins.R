# Project 5: Imbalanced Regression

# loading data
load("./data/train.Rdata")
load("./data/test.Rdata")

# install.packages("glmnet")
# install.packages("dplyr")
library(glmnet)
library(dplyr)

# selecting model y variable
train.rev <- train$totals.transactionRevenue
train.rev[which(is.na(train.rev))] <- 0
train.rev
# selecting model x variables (it's just all of them for now)
x.full <- train %>% select(-totals.transactionRevenue)

# LASSO regression w/ cross-validation (alpha = 1)
fit1 <- cv.glmnet(x.full, train.rev, alpha = 1)
