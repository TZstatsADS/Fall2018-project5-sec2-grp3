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
# selecting model x variables (removing NA's)
# finding NA's by percentage of rows with missing values
apply(train, 2, function(x) sum(is.na(x)) / length(x) * 100)
# selecting non-Y columns with fewer than half NA's or is a "totals" column
x.full <- train[, unname(colMeans(is.na(train)) < 0.5) | grepl("totals", names(train))]
# replacing NA's in "totals" columns with 0
x.totals <- x.full[, 9:13]
x.totals <- apply(x.totals, 2, function(x) replace(x, is.na(x), 0))
x.full[, 9:13] <- x.totals
# head(x.full, 5)
# formatting into a matrix
x.fullmat <- as.matrix(x.full)
x.fullmat <- na.omit(x.fullmat)
dim(x.fullmat) # we now have about 653k rows
y.rev <- as.numeric(x.fullmat[, 13])

# LASSO regression w/ cross-validation (alpha = 1)
fit1 <- cv.glmnet(x.fullmat, y.rev, alpha = 1)
