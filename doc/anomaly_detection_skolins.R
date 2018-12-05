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

# loading training data
load("./data/train.Rdata")
# saved as "train"

# creating a validation set from "train"
set.seed(100)
samp <- sample(nrow(train), 0.7 * nrow(train), replace = FALSE)
subtrain <- train[samp, ]
valid <- train[-samp, ]
summary(subtrain)
summary(valid)

# the detection function, AnomalyDetectionTs(), takes a 2-column dataframe of timestamps and
# univariate numerical data, so we need to preprocess our data a little
times.st <- as.POSIXct(subtrain$visitStartTime, origin = "1970-01-01 00:00.00 UTC")
revenue.st <- subtrain$totals.transactionRevenue
revenue.st[which(is.na(revenue.st))] <- 0
logrev.st <- ifelse(revenue.st != 0, log(revenue.st), 0)
anomtable.st <- cbind(times.st, logrev.st)
colnames(anomtable.st) <- c("Times", "Revenue")
head(anomtable.st)
tail(anomtable.st)

times.v <- as.POSIXct(valid$visitStartTime, origin = "1970-01-01 00:00.00 UTC")
revenue.v <- valid$totals.transactionRevenue
revenue.v[which(is.na(revenue.v))] <- 0
logrev.v <- ifelse(revenue.v != 0, log(revenue.v), 0)
anomtable.v <- cbind(times.v, logrev.v)
colnames(anomtable.v) <- c("Times", "Revenue")
head(anomtable.v)
tail(anomtable.v)