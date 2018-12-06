# Project 5: Imbalanced Regression
# Penalized LDA + LASSO/Ridge Two-Stage Models
# Sam Kolins (sk3651)

# this will effectively combine the penalized LDA and Stage 2 LASSO/Ridge code documents together
# although I will be removing some unnecessary lines here
# for classification performance metrics, see penalized_LDA_classification_skolins.R

# install.packages("penalizedLDA")
# install.packages("glmnet")
# install.packages("ggplot2")
library(penalizedLDA)
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

# creating classes for the data; 2 = paid, 1 = not paid (revenue = 0)
# the PenalizedLDA function prefers class labels that are (1, 2, ..., N), not (0, 1, ..., N)
paid.train <- ifelse(train$revenue > 0, 2, 1)

# creating model matrices
mdat <- model.matrix(revenue ~ ., data = dat)
train.mat <- mdat[samp, ]
test.mat <- mdat[-samp, ]

# running penalized LDA with standard LASSO/L1 penalties
penlda <- PenalizedLDA(train.mat[, -1], paid.train, xte = test.mat[, -1], lambda = 0.14, K = 1)

# true classes for test set
paid.test <- ifelse(test$revenue > 0, 2, 1)

# now time to feed the predicted paid customers into the regressers...
# isolating the predicted paid customers (PPC's)
index.ppc <- penlda$ypred == 2
index.ppc <- as.vector(index.ppc)
ppc <- test[index.ppc, ]
# this is now our test set for the regressers

# creating model matrix for the regressers
ppc.mat <- model.matrix(revenue ~ ., data = ppc)

# creating a test set from "ppc"
set.seed(100)
samp2 <- sample(nrow(ppc), 0.85 * nrow(ppc), replace = FALSE)
ppc.train <- ppc[samp2, ]
ppc.test <- ppc[-samp2, ]
summary(ppc.train)
summary(ppc.test)

# creating model matrices for the subsets above
ppctrain.mat <- ppc.mat[samp2, ]
ppctest.mat <- ppc.mat[-samp2, ]

# start with LASSO first
# linear LASSO cross-validation
cv.lasso <- cv.glmnet(ppctrain.mat[, -1], ppc.train$revenue, alpha = 1)
plot(cv.lasso)
lam.lasso <- cv.lasso$lambda.min
# LASSO regression
fit.lasso <- glmnet(ppctrain.mat[, -1], ppc.train$revenue, alpha = 1, lambda = lam.lasso)
fit.lasso$beta # coefficients
# training error
pred.lassotrain <- predict(fit.lasso, ppctrain.mat[, -1], s = lam.lasso) 
sqrt(mean((ppc.train$revenue - pred.lassotrain)^2))
# test error
pred.lassotest <- predict(fit.lasso, ppctest.mat[, -1], s = lam.lasso)
sqrt(mean((ppc.test$revenue - pred.lassotest)^2))
# these errors are MUCH worse than just running the regressers on all of the paid data
# that goes to show you why it's important to check on the full two-stage model
# but I'm blaming this lack of performance on pLDA more than I'm blaming it on LASSO/Ridge

# now we do Ridge
# linear Ridge cross-validation
cv.ridge <- cv.glmnet(ppctrain.mat[, -1], ppc.train$revenue, alpha = 0)
plot(cv.ridge)
lam.ridge <- cv.ridge$lambda.min
# linear Ridge regression #1 (bestlam2)
fit.ridge <- glmnet(ppctrain.mat[, -1], ppc.train$revenue, alpha = 0, lambda = lam.ridge)
fit.ridge$beta # coefficients
# training error
pred.ridgetrain <- predict(fit.ridge, ppctrain.mat[, -1], s = lam.ridge) 
sqrt(mean((ppc.train$revenue - pred.ridgetrain)^2))
# test error
pred.ridgetest <- predict(fit.ridge, ppctest.mat[, -1], s = lam.ridge)
sqrt(mean((ppc.test$revenue - pred.ridgetest)^2))

# metrics summary
lasso.errors <- c(sqrt(mean((ppc.train$revenue - pred.lassotrain)^2)), 
                  sqrt(mean((ppc.test$revenue - pred.lassotest)^2)))
ridge.errors <- c(sqrt(mean((ppc.train$revenue - pred.ridgetrain)^2)), 
                  sqrt(mean((ppc.test$revenue - pred.ridgetest)^2)))
metrics <- cbind(lasso.errors, ridge.errors)
colnames(metrics) <- c("LASSO", "Ridge")
rownames(metrics) <- c("Train", "Test")
metrics <- as.data.frame(metrics)
metrics

# the finale: what does the plot of predicted vs. true values look like for each model?
# remember: the equivalent plot for one-stage LASSO was terrible!
# you can find that one in figs/lasso_test_comparison_plot
test.compare <- cbind(ppc.test$revenue, pred.lassotest, pred.ridgetest)
IDs <- as.numeric(rownames(test.compare))
test.compare <- cbind(IDs, test.compare)
colnames(test.compare) <- c("ID", "True", "pLDA.LASSO", "pLDA.Ridge")
# LASSO plot
ggplot(data = as.data.frame(test.compare)) + 
  geom_point(mapping = aes(x = ID, y = pLDA.LASSO, color = "pLDA+LASSO"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("PPC pLDA+LASSO Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("pLDA+LASSO", "True"), values = c("red", "blue"))
# Ridge plot
ggplot(data = as.data.frame(test.compare)) + 
  geom_point(mapping = aes(x = ID, y = pLDA.Ridge, color = "pLDA+Ridge"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("PPC pLDA+Ridge Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("pLDA+Ridge", "True"), values = c("red", "blue"))

# WOW these plots are awful
# there are some quick improvements we can make though
# for instance, the predicted revenue for a customer classified as unpaid SHOULD BE ZERO
# if we add that data in, it might lower the overall MSE
# in fact, let's do that

# matrix of unpaid predicted customers (UPPC's)
uppc <- test[-index.ppc, ]

# vectors of final predictions
# this is a bit tricky because we actually have three groups:
# 1. points classified as "unpaid"
# 2. points classified as "paid" but used in the training set
# 3. points classified as "paid" but used in the test set
# we will first create a vector that indicates which of the above groups each point is in
# they are numbered in the order they are listed above
unpaid.nums <- which(penlda$ypred == 1)
paid.nums <- which(penlda$ypred == 2)
paid.nums.train <- paid.nums[samp2]
paid.nums.test <- paid.nums[-samp2]
# the actual group assignment vector now
groups <- rep(0, nrow(test))
groups[unpaid.nums] <- 1
groups[paid.nums.train] <- 2
groups[paid.nums.test] <- 3
groups
# of course, paid training set data isn't regressed at all, so that can be dropped
regressed <- which(groups != 2)
length(regressed) # the TRUE number of predictions made

# final predictions matrix
finalpreds <- cbind(1:nrow(test), groups)
finalpred.lasso <- rep(0, nrow(finalpreds))
finalpred.lasso[groups == 3] <- pred.lassotest
finalpred.lasso[groups == 2] <- NA
finalpred.ridge <- rep(0, nrow(finalpreds))
finalpred.ridge[groups == 3] <- pred.ridgetest
finalpred.ridge[groups == 2] <- NA
finalpreds <- cbind(finalpreds, finalpred.lasso, finalpred.ridge)
finalpreds <- cbind(finalpreds, test$revenue)
colnames(finalpreds) <- c("ID", "Group", "pLDA.LASSO", "pLDA.Ridge", "True")
finalpreds <- as.data.frame(finalpreds)
finalpreds <- finalpreds[groups != 2, ]
rownames(finalpreds) <- 1:nrow(finalpreds)

# before we plot, let's compute the TRUE MSE
final.lasso.mse <- sqrt(mean((finalpreds$True - finalpreds$pLDA.LASSO)^2))
final.ridge.mse <- sqrt(mean((finalpreds$True - finalpreds$pLDA.Ridge)^2))
# !!!!! incredible! both MSE's are around 1.342!

# but now, let's look at the plots
# LASSO plot
ggplot(data = finalpreds) + 
  geom_point(mapping = aes(x = ID, y = pLDA.LASSO, color = "pLDA+LASSO"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("Final pLDA+LASSO Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("pLDA+LASSO", "True"), values = c("red", "blue"))
# Ridge plot
ggplot(data = finalpreds) + 
  geom_point(mapping = aes(x = ID, y = pLDA.Ridge, color = "pLDA+Ridge"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("Final pLDA+Ridge Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("pLDA+Ridge", "True"), values = c("red", "blue"))
# sadly, the plots are still terrible
# but most of the red points are hiding behind the big blue line at 0
# also, both Ridge and LASSO sometimes predict negative values for log revenue?!
# there's a lot of room for improvement in these models for sure
# but technically, it has a better MSE than XGBoost + Tweedie!