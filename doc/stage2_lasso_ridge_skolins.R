# Project 5: Imbalanced Regression
# LASSO/Ridge Regression in Second Stage (Post-Classification)
# Sam Kolins (sk3651)

# I'm running these regressions on all of the true paid data...
# ...but in main.Rmd, when we evaluate the two-stage algorithms, they should be run on the data
# that was CLASSIFIED AS "PAID" by each algorithm, not the TRUE paid data
# this is just an analysis of how well each algorithm does in a BEST-CASE SCENARIO

# install.packages("glmnet")
library(glmnet)

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
sqrt(mean(ptrain$revenue - pred.lassotrain)^2)
# test error
pred.lassotest <- predict(fit.lasso, pvalid.mat[, -1], s = bestlam)
sqrt(mean(pvalid$revenue - pred.lassotest)^2)

# linear LASSO regression #2 (lam.1se)
fit.lasso2 <- glmnet(ptrain.mat[, -1], ptrain$revenue, alpha = 1, lambda = lam.1se)
fit.lasso2$beta # coefficients
# training error
pred.lassotrain2 <- predict(fit.lasso, ptrain.mat[, -1], s = lam.1se) 
sqrt(mean(ptrain$revenue - pred.lassotrain)^2)
# test error
pred.lassotest2 <- predict(fit.lasso, pvalid.mat[, -1], s = lam.1se)
sqrt(mean(pvalid$revenue - pred.lassotest)^2)
# the training and test errors are about the same, but this model has a much higher degree
# of shrinkage (far more coefficients shrunk to zero)