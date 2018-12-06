setwd('C:/Users/Deepika/Documents/ADS/Project 5/Fall2018-project5-sec2proj5-grp3/data')
load('test.RData')
load('train.RData')

#Run the preprocessing script

require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')

nsamp<-sample(1:nrow(dat),round(0.8*nrow(dat)))
trainset<-dat[nsamp,]
testset<-dat[-nsamp,]

ntdf<-sparse.model.matrix(revenue~.,data=trainset)

library(data.table)
library(mltools)
library(vtreat)
rows<-nrow(ntdf)


#removing variables not of interest
#ntdf<-one_hot(as.data.table(tdf[,-c(7:11,16:17)])) 

#set.seed(4623762)
# tplan <- vtreat::designTreatmentsZ(ntdf[1:rows,], colnames(ntdf),minFraction= 0,verbose=FALSE)
# trainVtreat <- as.matrix(vtreat::prepare(tplan,ntdf[1:rows,]))
# print(dim(trainVtreat))                                 
# print(colnames(trainVtreat))

#Building the CV XGBoost function:
library("xgboost")
library("sigr")
library("WVPlots")
library("vtreat")

set.seed(4623762)
crossValPlan <- vtreat::kWayStratifiedY(rows, 
                                        10, 
                                        trainset, 
                                        trainset$revenue)

#ntdf[which(is.na(ntdf$totals.transactionRevenue)),]<-0

#https://github.com/dmlc/xgboost/issues/332
#https://www.statworx.com/ch/blog/xgboost-tree-vs-linear/
#Function definition:
evaluateModelingProcedure <- function(xMatrix, outcomeV, crossValPlan) {
                               preds <- rep(NA_real_, nrow(xMatrix))
                               for(ci in crossValPlan) {
                                   nrounds <- 100
                                   cv <- xgb.cv(data= xMatrix[ci$train, ],
                                   #cv <- xgb.cv(data=ntdf,
                                                 label=outcomeV[ci$train],
                                                 objective= 'reg:linear',
                                                  eval_metric = "rmse",
                                                  max_depth=20,
                                                  nrounds= nrounds,
                                                  verbose= 0,
                                                   nfold= 5)
                                  nrounds  <- which.min(cv$evaluation_log$test_rmse_mean) # regression
                                  model <- xgboost(data= xMatrix[ci$train, ],
                                  #model <- xgboost(data= ntdf,
                                                   label= outcomeV[ci$train],
                                                   objective= 'reg:linear',
                                                   max_depth=20,
                                                   eval_metric='rmse',
                                                   nrounds= nrounds,
                                                   verbose= 0)
    
                                  preds[ci$app] <-  predict(model, xMatrix[ci$app, ])
                                  #preds <-  predict(model, ntdf)
                               }
                               preds
                            }

trainset$predVtreatZ <- evaluateModelingProcedure(ntdf,
                                              trainset$revenue,
                                              crossValPlan)
rmse<-sqrt(mean(trainset$predVtreatZ-trainset$revenue)^2)
rmse

#Test error
crossValPlanTest <- vtreat::kWayStratifiedY(nrow(testset), 
                                        10, 
                                        testset, 
                                        testset$revenue)
ntdfTest<-sparse.model.matrix(revenue~.,data=testset)
testset$predVtreatZ <- evaluateModelingProcedure(ntdfTest,
                                                  testset$revenue,
                                                  crossValPlanTest)
rmseTest<-sqrt(mean(testset$predVtreatZ-testset$revenue)^2)
rmseTest

library(ggplot2)

ggplot(data = as.data.frame(testset)) + 
  geom_point(mapping = aes(x = c(1:nrow(testset)), y = predVtreatZ, color = "Predicted"), alpha = 0.3) +
  geom_point(mapping = aes(x = c(1:nrow(testset)), y = revenue, color = "True"), alpha = 0.3) +
  ylab("Log Revenue") + scale_color_manual("Legend", 
                                           breaks = c("Predicted", "True"),
                                           values = c("red", "blue"))
  