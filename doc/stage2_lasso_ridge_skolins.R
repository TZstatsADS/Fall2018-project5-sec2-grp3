# Project 5: Imbalanced Regression
# LASSO/Ridge Regression in Second Stage (Post-Classification)
# Sam Kolins (sk3651)

# I'm running these regressions on all of the true paid data...
# ...but in main.Rmd, when we evaluate the two-stage algorithms, they should be run on the data
# that was CLASSIFIED AS "PAID" by each algorithm, not the TRUE paid data
# this is just an analysis of how well each algorithm does in a BEST-CASE SCENARIO

# install.packages("glmnet")
# install.packages("ggplot2")
library(glmnet)
library(ggplot2)

# loading preprocessed data
load("./output/cleaned.Rdata")
# saved as "dat"
# revenue is log revenue of the original data set

# subsetting only paid customers
index <- dat$revenue > 0
paid <- dat[index, ]
paid <- as.data.frame(paid)

# creating model matrix
paid.mat <- model.matrix(revenue ~ ., data = paid)

# creating a test set from "paid"
set.seed(100)
samp <- sample(nrow(paid), 0.85 * nrow(paid), replace = FALSE)
ptrain <- paid[samp, ]
pvalid <- paid[-samp, ]
summary(ptrain)
summary(pvalid)

# creating model matrices for the subsets above
ptrain.mat <- paid.mat[samp, ]
pvalid.mat <- paid.mat[-samp, ]

# linear LASSO cross-validation
cv.lasso <- cv.glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 1)
plot(cv.lasso)
bestlam <- cv.lasso$lambda.min
lam.1se <- cv.lasso$lambda.1se

# linear LASSO regression #1 (bestlam)
fit.lasso <- glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 1, lambda = bestlam)
fit.lasso$beta # coefficients
# training error
pred.lassotrain <- predict(fit.lasso, ptrain.mat[, -1], s = bestlam) 
sqrt(mean((ptrain$revenue - pred.lassotrain)^2))
# test error
pred.lassotest <- predict(fit.lasso, pvalid.mat[, -1], s = bestlam)
sqrt(mean((pvalid$revenue - pred.lassotest)^2))

# linear LASSO regression #2 (lam.1se)
fit.lasso2 <- glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 1, lambda = lam.1se)
fit.lasso2$beta # coefficients
# training error
pred.lassotrain2 <- predict(fit.lasso, ptrain.mat[, -1], s = lam.1se) 
sqrt(mean((ptrain$revenue - pred.lassotrain)^2))
# test error
pred.lassotest2 <- predict(fit.lasso, pvalid.mat[, -1], s = lam.1se)
sqrt(mean((pvalid$revenue - pred.lassotest)^2))
# the training and test errors are about the same, but this model has a much higher degree
# of shrinkage (far more coefficients shrunk to zero)
# that might just be due to the specific seed I used though

# linear Ridge cross-validation
cv.ridge <- cv.glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 0)
plot(cv.ridge)
bestlam2 <- cv.ridge$lambda.min
lam2.1se <- cv.ridge$lambda.1se

# linear Ridge regression #1 (bestlam2)
fit.ridge <- glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 0, lambda = bestlam2)
fit.ridge$beta # coefficients
# training error
pred.ridgetrain <- predict(fit.ridge, ptrain.mat[, -1], s = bestlam2) 
sqrt(mean((ptrain$revenue - pred.ridgetrain)^2))
# test error
pred.ridgetest <- predict(fit.ridge, pvalid.mat[, -1], s = bestlam2)
sqrt(mean((pvalid$revenue - pred.ridgetest)^2))

# linear Ridge regression #2 (lam2.1se)
fit.ridge2 <- glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 0, lambda = lam2.1se)
fit.ridge2$beta # coefficients
# training error
pred.ridgetrain2 <- predict(fit.ridge, ptrain.mat[, -1], s = lam2.1se) 
sqrt(mean((ptrain$revenue - pred.ridgetrain2)^2))
# test error
pred.ridgetest2 <- predict(fit.ridge, pvalid.mat[, -1], s = lam2.1se)
sqrt(mean((pvalid$revenue - pred.ridgetest2)^2))

# can we get a visual sense for how accurate our predictions are?
IDs <- rownames(test.compare)
test.compare <- cbind(as.numeric(IDs), pvalid$revenue, pred.lassotest, pred.ridgetest)
colnames(test.compare) <- c("ID", "True", "LASSO", "Ridge")
# LASSO plot
ggplot(data = as.data.frame(test.compare)) + 
  geom_point(mapping = aes(x = ID, y = LASSO, color = "LASSO"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("LASSO Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("LASSO", "True"), values = c("red", "blue"))
# Ridge plot
ggplot(data = as.data.frame(test.compare)) + 
  geom_point(mapping = aes(x = ID, y = Ridge, color = "Ridge"), alpha = 0.3) +
  geom_point(mapping = aes(x = ID, y = True, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + ggtitle("Ridge Predictions vs. True Revenues") +
  scale_color_manual("Legend", breaks = c("Ridge", "True"), values = c("red", "blue"))
# the plots depict a worse fit than the MSE's suggest
# but numerically, it's not much worse than XGBoost or splines...