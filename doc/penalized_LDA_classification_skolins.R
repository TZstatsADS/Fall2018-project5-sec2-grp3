# Project 5: Imbalanced Regression
# Classification via Penalized LDA
# Sam Kolins (sk3651)

# install.packages("penalizedLDA")
library(penalizedLDA)
# install.packages("psych")
library(psych)
# this is just for computing harmonic mean
# install.packages("irr")
library(irr)
# for Cohen's kappa
# install.packages("pROC")
library(pROC)
# for ROC curve

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

# creating classes for the data; 2 = paid, 1 = not paid (revenue = 0)
# the PenalizedLDA function prefers class labels that are (1, 2, ..., N), not (0, 1, ..., N)
paid.train <- ifelse(train$revenue > 0, 2, 1)
# check out how imbalanced the data is!
table(paid.train) / length(paid.train)

# creating model matrices
train.mat <- model.matrix(revenue ~ ., data = train)
test.mat <- model.matrix(revenue ~ ., data = test)

# running penalized LDA with standard LASSO/L1 penalties
penlda <- PenalizedLDA(train.mat[, -1], paid.train, xte = test.mat[, -1], lambda = 0.14, K = 1)

# true classes for test set
paid.test <- ifelse(test$revenue > 0, 2, 1)
# underlying distribution of test classes is extreme as well
table(paid.test) / length(paid.test)

# metrics
## accuracy
acc <- sum(penlda$ypred == paid.test) / length(paid.test)
acc
# 97.7%, but that doesn't tell us much

## confusion matrix
conf <- table(penlda$ypred, paid.test)
conf
# [1,1] true negative, [1,2] false negative
# [2,1] false positive, [2,2] true positive

## precision and recall (for the minority class of paid consumers)
prec <- conf[2, 2] / sum(conf[2, ])
prec
# only 30.7% :^(
rec <- conf[2, 2] / sum(conf[, 2])
rec
# 61.7%, which is okay

## precision and recall (for the majority class of unpaid consumers)
prec.major <- conf[1, 1] / sum(conf[1, ])
prec.major
# about 99.5%!
rec.major <- conf[1, 1] / sum(conf[, 1])
rec.major
# about 98.1%!

## table summarizing the above findings
metrics <- matrix(c(prec.major, rec.major, prec, rec), nrow = 2, byrow = TRUE)
rownames(metrics) <- c(1, 2)
colnames(metrics) <- c("Precision", "Recall")
metrics

## F-1 scores
f1 <- harmonic.mean(c(prec, rec))
f1.major <- harmonic.mean(c(prec.major, rec.major))
metrics <- cbind(metrics, c(f1.major, f1))
colnames(metrics) <- c("Precision", "Recall", "F1-Score")
metrics
# F1 score of the minority class is 41%

## Cohen's kappa
# measures level of agreement between predictions and true classes factoring in imbalance ratios
# can be thought of as an "imbalance-adjusted accuracy level"
comparison <- cbind(penlda$ypred, paid.test)
kappa2(comparison)
# here, kappa = 0.4 (40%), which feels like it better captures the performance metrics

## ROC curve
resp <- paid.test - 1
pred <- as.vector(penlda$ypred - 1)
# roc() function prefers 0 and 1 as binary classes rather than 1 and 2
roc.curve <- roc(resp, pred)
plot(roc.curve)
auc(roc.curve)
# AUC = 0.7995, which is not terrible. The classifier is a good bit better than random chance

# performance may be poor because our numerical variables are not likely to be normal/Gaussian
# we can try optimizing lambda though to see if the metrics are any better there

# first pass:
lams1 <- c(1e-4, 1e-3, 0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10)
cv1 <- PenalizedLDA.cv(train.mat[, -1], paid.train, lambdas = lams1, K = 1)
# mean cross-validation errors
cv1$errs
goodlam1 <- cv1$bestlambda
goodlam1.se <- cv1$bestlambda.1se

# second (and final) pass:
lams2 <- seq(goodlam1, goodlam1.se, length.out = 10)
cv2 <- PenalizedLDA.cv(train.mat[, -1], paid.train, lambdas = lams2, K = 1)
# mean cross-validation errors (they are very close to each other)
cv2$errs
bestlam <- cv2$bestlambda

# running another penalized LDA with the new optimized lambda
penlda2 <- PenalizedLDA(train.mat[, -1], paid.train, 
                        xte = test.mat[, -1], lambda = bestlam, K = 1)
# new metric table and confusion matrix
conf2 <- table(penlda2$ypred, paid.test)
conf2
# ...the confusion matrix has only one row for predictions.
# that means it said every point belonged to class 1
# it's automatically terrible

# so how do we fix this?
# well, we can try penalized logistic regression, which does not depend on any underlying
# distribution assumptions in our data
# this is good because our data has really awful distributions due to imbalance; the data
# doesn't start to look nice until we remove all the unpaid customers
