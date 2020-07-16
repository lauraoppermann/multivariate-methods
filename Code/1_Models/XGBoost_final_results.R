setwd("H:/multivariate-methods/Code")
load("./Data_prep/train_val_data.Rdata")
load("./Data_prep/test_data.Rdata")


library("mlr")
library("magrittr")
library("dplyr")
library("xgboost")

library("parallelMap")
parallelStartSocket(20)

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
cal = generateCalibrationData(pred_test,breaks = seq(from= 0.0, to =1, by = 0.025))
plotCalibration(cal, rag=FALSE)


##try this for calibration plot
library("classifierplots")
classifierplots::calibration_plot(pred_test$data$truth, pred_test$data$prob.1)



#############################################################################
#### COMPUTING AVG BENFITS  with optimal threshold strategy #################
#############################################################################
#function to compute avg costs for optimal probability choice for different costs
#preds: mlr prediction object 
#costs: cost of default (other costs therefore normalized to 1)
comp_cost = function(costs,preds){ 
  th = 1/(1+ costs)
  cost_matrix = matrix(c(0,-costs,0, +1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_matrix) = c("1","0")
  colnames(cost_matrix) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.costs = mlr::makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = cost_matrix)
  return(mlr::performance(preds, measures = credit.costs))
}

#function to compute avg costs if bank does not give loans
#preds: mlr prediction object 
#costs: cost of default (other costs therefore normalized to 1)

comp_cost_th_0 = function(costs,preds){ 
  th = 0
  cost_matrix = matrix(c(0,-costs,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_matrix) = c("1","0")
  colnames(cost_matrix) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.costs = mlr::makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = cost_matrix)
  return(mlr::performance(preds, measures = credit.costs))
}

#function to compute avg costs if bank does not give any loan
#preds: mlr prediction object 
#costs: cost of default (other costs therefore normalized to 1)

comp_cost_th_1 = function(costs,preds){ 
  th = 1
  cost_matrix = matrix(c(0,-costs,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
  rownames(cost_matrix) = c("1","0")
  colnames(cost_matrix) = c("1","0")
  preds = mlr::setThreshold(preds, th)
  credit.costs = mlr::makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = cost_matrix)
  return(mlr::performance(preds, measures = credit.costs))
}


costs_l = list(1,2,3.5,4,5,10, 15, 20, 25, 30,35, 40,60, 80)
sapply(costs_l, function(x) comp_cost(costs = x, pred_test))
sapply(costs_l, function(x) comp_cost_th_0(costs = x, pred_test))
sapply(costs_l, function(x) comp_cost_th_1(costs= x, pred_test))



###rest is experimental

d = generateThreshVsPerfData(pred_test, measures = list(credit.costs))
plotThreshVsPerf(d, mark.th = 1/3)

###############################################################################################################
### Tune threshold 
###############################################################################################################

#1.st compute 


# 3-fold cross-validation
credit.costs = mlr::makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = cost_matrix, minimize = FALSE)
rdesc = makeResampleDesc("CV", iters = 4)
r = resample(classif.lrn, classif.task, resampling = rdesc, show.info = FALSE)


test = tuneThreshold(r$pred , measure = credit.costs )


cost_matrix = matrix(c(0,-3,0, 1), nrow=2,ncol = 2,  byrow=TRUE)
rownames(cost_matrix) = c("1","0")
colnames(cost_matrix) = c("1","0")

parallelStop()
