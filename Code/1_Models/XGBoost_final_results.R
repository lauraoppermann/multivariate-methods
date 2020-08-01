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
save(mod, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/trained_model_xgboost.Rdata")
save(pred_test, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/predictions_xgboost.Rdata")




#############################################################################
#### COMPUTING AVG BENFITS  with optimal threshold strategy #################
#############################################################################
#function to compute avg costs for optimal probability choice for different costs
#df: data frame with fitting data
#preds: mlr prediction object 
#costs: costs of default (benefits of acquiring a "good" customer is normalized to 1)



#Compute costs
#function to compute costs when lending to every one
comp_weight_benefit_th1 = function(df,pred,  costs){
  th = 1
  c_r = 1/costs
  pred = mlr::setThreshold(pred, th)
  cost_mat = cbind((as.numeric(df$Target_def)-1==0)*c_r*df$Q_this_loan -(as.numeric(df$Target_def)-1==1)*(df$Q_this_loan))
  costs = sum(cost_mat*((pred$data$truth== 1 & pred$data$response== 0) + (pred$data$truth== 0 & pred$data$response== 0)))
  return(costs / nrow(df))}


#function to compute costs when lending according to optimal decision threshold
comp_weight_benefit = function(df,pred,  costs){
  th =  1/(1+ costs)
  c_r = 1/costs
  pred = mlr::setThreshold(pred, th)
  cost_mat = cbind((as.numeric(df$Target_def)-1==0)*c_r*df$Q_this_loan -(as.numeric(df$Target_def)-1==1)*(df$Q_this_loan))
  costs = sum(cost_mat*((pred$data$truth== 1 & pred$data$response== 0) + (pred$data$truth== 0 & pred$data$response== 0)))
  return(costs / nrow(df))}

costs_l = as.list(1:20)
benef_xgboost = sapply(costs_l, function(x) comp_weight_benefit( df_test, pred_test,costs = x))
benef_naive =   sapply(costs_l, function(x) comp_weight_benefit_th1( df_test, pred_test,costs = x))
names(benef_xgboost) = 1:20
names(benef_naive) = 1:20
save(benef_xgboost, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_xgboost.Rdata")
save(benef_naive, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_naive.Rdata")

#compute number of loans awarded: 
n_loans_xgboost = lapply(1:20, function(x) sum(pred_test$data$prob.1 < (1/(1+x))))
save(n_loans_xgboost, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/n_loans_xgboost.Rdata")


