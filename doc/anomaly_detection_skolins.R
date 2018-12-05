# Project 5: Imbalanced Regression
# Classification via Anomaly Detection
# Sam Kolins (sk3651)

# the following package, created by Twitter (originally for the purpose of analyzing photos
# uploaded during the holidays), includes tools for anomaly detection analysis
# the main algorithm used in this package is seasonal-hybrid ESD (ESD = extreme studentized
# deviate), which builds upon the generalized ESD test
library(devtools)
# install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

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