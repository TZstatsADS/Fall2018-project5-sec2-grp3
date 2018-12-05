# Project 5: Imbalanced Regression
# Classification via Penalized Logistic Regression
# Sam Kolins (sk3651)

# install.packages("glmnet")
library(glmnet)

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
paid.train <- ifelse(train$revenue > 0, 1, 0)
# 1 = paid, 0 = unpaid

# using L1/LASSO penalization for logistic regression (choosing lambda via CV)
# be patient, this takes some time!
cv.lasso <- cv.glmnet(train.mat[, -1], paid.train, family = "binomial", alpha = 1)
minlam.lasso <- cv.lasso$lambda.min
plr.lasso <- glmnet(train.mat[, -1], paid.train, family = "binomial", 
                    lambda = minlam.lasso, alpha = 1)
# convergence not reached for the ideal lambda after max iterations
# gave lambda = Inf so I'm not really sure what's going on here