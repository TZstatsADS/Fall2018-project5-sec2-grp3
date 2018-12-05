# Project 5: Imbalanced Regression
# Using LASSO Regression using Cleaned Data from preprocessing.R

library(glmnet)
library(ggplot2)

# loading preprocessed data
load("./output/cleaned.Rdata")
# saved as "dat"
# revenue is log revenue of the original data set

# creating a test set from "dat" (which is all training data of the original data set)
set.seed(100)
samp <- sample(nrow(dat), 0.7 * nrow(dat), replace = FALSE)
train <- dat[samp, ]
test <- dat[-samp, ]
summary(train)
summary(test)

# creating model matrix from training data
train.mat <- model.matrix(revenue ~ ., data = train)

# creating y variable
logrev.train <- train$revenue

# running ElasticNet/LASSO regression (alpha = 1)
fit1 <- cv.glmnet(train.mat, logrev.train, alpha = 1)
fit1
plot(fit1)
opt.lambda <- fit1$lambda.min

# running LASSO with optimal lambda
fit1.opt <- glmnet(train.mat[, -1], logrev.train, family = "gaussian", alpha = 1, lambda = opt.lambda)
plot(glmnet(train.mat[, -1], logrev.train, family = "gaussian", alpha = 1), xvar = "lambda", label = TRUE)
# here are the coefficients. many of them have been shrunk to zero, but almost all of them are
# low anyway (< 10^-1, many < 10^-2)
coeffs <- fit1.opt$beta
coeffs
plot(coef(fit1.opt, s = opt.lambda)) # coeff plot
# only 17% of the deviance is explained by this model
fit1.opt$dev.ratio

pred1.train <- predict(fit1.opt, train.mat[, -1], s = opt.lambda)
sqrt(mean(logrev.train - pred1.train)^2) # training error (root MSE) is extremely low!
# overfitting is likely occurring

test.mat <- model.matrix(revenue ~ ., data = test)
pred1.test <- predict(fit1.opt, s = opt.lambda, newx = test.mat[, -1], type = "response")
logrev.test <- test$revenue
sqrt(mean(logrev.test - pred1.test)^2) # test error (root MSE) really low???

# let's visually see if the test predictions are actually good
# the following is a matrix that stores the ID's of the data points, the true log revenue,
# and the predicted log revenue
test.compare <- cbind(logrev.test, pred1.test)
indices <- rownames(test.compare)
test.compare <- cbind(as.numeric(indices), test.compare)
colnames(test.compare) <- c("ID", "True", "Predicted")
rownames(test.compare) <- NULL
head(test.compare)

ggplot(data = as.data.frame(test.compare)) + 
  geom_point(mapping = aes(x = ID, y = Predicted, color = "Predicted"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + scale_color_manual("Legend", 
                                           breaks = c("Predicted", "True"),
                                           values = c("red", "blue"))
