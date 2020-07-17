setwd("C:/Users/felix/Documents/GitHub/multivariate-methods/Code")
load("./Data_prep/train_val_data.Rdata")
load("./Data_prep/test_data.Rdata")


library("mlr")
library("magrittr")
library("dplyr")
library("xgboost")

library("parallelMap")
parallelStartSocket(5)

## Change certain factors to dummy variables
df_train_val = createDummyFeatures(
  df_train_val,
  target = "Target_def",
  method = "1-of-n",
  cols = c('C_Job','C_Score_Desc','C_Id_Type')
)

df_test = createDummyFeatures(
  df_test,
  target = "Target_def",
  method = "1-of-n",
  cols = c('C_Job','C_Score_Desc','C_Id_Type')
)


#create task
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df_train_val, 
                               target = "Target_def",
                               positive = "1") # specify train, validation


#specify learner (with parameters found in tuning round)
classif.lrn = makeLearner("classif.xgboost", 
                          par.vals = list(eta=0.01, 
                                          nrounds=1500,
                                          lambda=1.5, 
                                          subsample= 0.6,
                                          colsample_bytree=0.6,
                                           eval_metric ="auc"),
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)


#train on entire train_val data
mod = train(classif.lrn, task = classif.task )
pred_test = predict(mod, makeClassifTask(data = df_test,  target = "Target_def"))

#compute performance on test and plot
mlr::performance(pred_test, mlr::auc)
df = generateThreshVsPerfData(pred_test, measures = list(fpr, tpr, mmce))
plotROCCurves(df)

#compute calibration
cal = generateCalibrationData(pred_test,breaks = seq(from= 0.0, to =1, by = 0.05))
plotCalibration(cal, rag=FALSE)


##try this for calibration plot
library("classifierplots")
classifierplots::calibration_plot(pred_test$data$truth, pred_test$data$prob.1)



#############################################################################
#### COMPUTING AVG BENFITS  with optimal threshold strategy #################
#############################################################################
#function to compute avg costs for optimal probability choice for different costs
#preds: mlr prediction object 
#costs: costs of default (benefits of acquiring a "good" customer is normalized to 1)
comp_benef = function(costs,preds){ 
  th = 1/(1+ costs)
  cost_benefit_matrix = matrix(c(0,-costs,0, +1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_benefit_matrix) = c("1","0")
  colnames(cost_benefit_matrix) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.benef = mlr::makeCostMeasure(id = "credit.benef", name = "Credit Benefits", costs = cost_benefit_matrix)
  return(mlr::performance(preds, measures = credit.benef))
}

#function to compute avg costs if bank does not give loans
#preds: mlr prediction object 
#costs: cost of default (other costs therefore normalized to 1)

comp_benef_th_0 = function(costs,preds){ 
  th = 0
  cost_benefit_matrix = matrix(c(0,-costs,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_benefit_matrix) = c("1","0")
  colnames(cost_benefit_matrix) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.benef = mlr::makeCostMeasure(id = "credit.benef", name = "Credit Benefits", costs = cost_benefit_matrix)
  return(mlr::performance(preds, measures = credit.benef))
}

#function to compute avg costs if bank does not give any loan
#preds: mlr prediction object 
#costs: cost of default (other costs therefore normalized to 1)

comp_benef_th_1 = function(costs,preds){ 
  th = 1
  cost_benefit_matrix = matrix(c(0,-costs,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_benefit_matrix ) = c("1","0")
  colnames(cost_benefit_matrix ) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.benef = mlr::makeCostMeasure(id = "credit.benef", name = "Credit Benefits", costs = cost_benefit_matrix)
  return(mlr::performance(preds, measures = credit.benef))
}

#compute benefits for varying costs of default
costs_l = as.list(1:20)
benef_xgboost = sapply(costs_l, function(x) comp_benef(costs = x, pred_test))
#sapply(costs_l, function(x) comp_benef_th_0(costs = x, pred_test))
benef_naive = sapply(costs_l, function(x) comp_benef_th_1(costs= x, pred_test))

save(benef_xgboost, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_xgboost.Rdata")
save(benef_naive, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_naive.Rdata")

###Example of maximization on test
cost_benefit_matrix = matrix(c(0,-6,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
rownames(cost_benefit_matrix) = c("1","0")
colnames(cost_benefit_matrix) = c("1","0")
credit.benef = mlr::makeCostMeasure(id = "credit.benef", name = "Credit Benefits", costs = cost_benefit_matrix, minimize = FALSE)

d = generateThreshVsPerfData(pred_test, measures = list(credit.benef))
plotThreshVsPerf(d, mark.th = 1/7)


parallelStop()
