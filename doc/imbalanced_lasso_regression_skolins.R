# Project 5: Imbalanced Regression
# Using LASSO Regression using Cleaned Data from preprocessing.R

library(glmnet)

# loading preprocessed data
load("./output/trainclean.Rdata")
# saved as "dat"
# revenue is log revenue of the original data set

# creating a test set from "dat" (which is all training data of the original data set)
set.seed(100)
samp <- sample(nrow(dat), 0.7 * nrow(dat), replace = FALSE)
train <- dat[samp, ]
test <- dat[-samp, ]
summary(train)
summary(test)

# only one data point in "train" has os == "NTT DoCoMo" and a different data point has
# campaign == "Data Share"
singletons <- c(which(train$os == "NTT DoCoMo"), which(train$campaign == "Data Share"))
# we'll get rid of them because otherwise, since "test" does not have data with these factors,
# we get a dimensionality error when predicting over the test model matrix
train <- train[-singletons, ]

# creating model matrix from training data
train.mat <- model.matrix(revenue ~ ., data = train)

# creating y variable
logrev <- train$revenue

# running ElasticNet/LASSO regression (alpha = 1)
fit1 <- cv.glmnet(train.mat, logrev, alpha = 1)
fit1
plot(fit1)
opt.lambda <- fit1$lambda.min

# running LASSO with optimal lambda
fit1.opt <- glmnet(train.mat[, -1], logrev, family = "gaussian", alpha = 1, lambda = opt.lambda)
plot(glmnet(train.mat[, -1], logrev, family = "gaussian", alpha = 1), xvar = "lambda", label = TRUE)
# here are the coefficients. many of them have been shrunk to zero, but almost all of them are
# low anyway (< 10^-1, many < 10^-2)
coeffs <- fit1.opt$beta
coeffs
plot(coef(fit1.opt, s = opt.lambda)) # coeff plot
# only 17% of the deviance is explained by this model
fit1.opt$dev.ratio

pred1.train <- predict(fit1.opt, train.mat[, -1], s = opt.lambda)
sqrt(mean(logrev - pred1.train)^2) # training error (root MSE) is extremely low!
# overfitting is likely occurring

test.mat <- model.matrix(revenue ~ ., data = test)
pred1.test <- predict(fit1.opt, s = opt.lambda, newx = test.mat, type = "response")
