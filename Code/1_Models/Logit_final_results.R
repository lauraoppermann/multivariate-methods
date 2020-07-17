setwd("C:/Users/felix/Documents/GitHub/multivariate-methods/Code")
load("./Data_prep/train_val_data.Rdata")
load("./Data_prep/test_data.Rdata")


library("mlr")
library("LiblineaR")
library("magrittr")
library("dplyr")
#library("caret")
library("parallelMap")
parallelStartSocket(4)



######################################################
#### preprocess train_val data ######################
######################################################

#data preprocess specific to logit

#add squared variables 
squares = df_train_val %>%
  select(Q_this_loan,
         Q_Balance,
         Q_Month_Pay,
         Q_AVG_loan_Age,
         Q_Credit_hist_length,
         N_recent_loans,
         N_Loans_Paid,
         Q_Loans_Paid_To_Tot,
         Q_Act_Debt_To_Total,
         Q_Loan_to_Balance,
         N_Loan_Applis,
         N_Age,
         N_Loans_Tot,
         N_Loans_Active,
         N_Loans_Overdue) %>%
  mutate_all(function(x) x^2)


df_train_val = dplyr::bind_cols(df_train_val,squares)

#adding interaction terms (specific to logit)
df_train_val %<>% dplyr::mutate(Q_this_loan_X_Q_loan_to_value = Q_this_loan * Q_loan_to_value,
                                Q_this_loan_X_N_Age = Q_this_loan * N_Age  ,
                                Q_this_loan_X_Q_score = Q_this_loan * Q_Score,
)

#change factor to dummy
df_train_val = createDummyFeatures(
  df_train_val,
  target = "Target_def",
  method = "1-of-n",
  cols = NULL
)


######################################################
#### preprocess test data       ######################
######################################################

#data preprocess specific to logit

#add squared variables 
squares = df_test %>%
  select(Q_this_loan,
         Q_Balance,
         Q_Month_Pay,
         Q_AVG_loan_Age,
         Q_Credit_hist_length,
         N_recent_loans,
         N_Loans_Paid,
         Q_Loans_Paid_To_Tot,
         Q_Act_Debt_To_Total,
         Q_Loan_to_Balance,
         N_Loan_Applis,
         N_Age,
         N_Loans_Tot,
         N_Loans_Active,
         N_Loans_Overdue) %>%
  mutate_all(function(x) x^2)


df_test = dplyr::bind_cols(df_test,squares)

#adding interaction terms (specific to logit)
df_test %<>% dplyr::mutate(Q_this_loan_X_Q_loan_to_value = Q_this_loan * Q_loan_to_value,
                                Q_this_loan_X_N_Age = Q_this_loan * N_Age  ,
                                Q_this_loan_X_Q_score = Q_this_loan * Q_Score,
)

#change factor to dummy
df_test = createDummyFeatures(
  df_train_val,
  target = "Target_def",
  method = "1-of-n",
  cols = NULL
)


##################################################################################
######## fit model with previously tuned parameters     #########################
#################################################################################

#specify cross validation with necessary steps:
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df_train_val, 
                               target = "Target_def",
                               positive = "1")

classif.lrn = makeLearner("classif.LiblineaRL1LogReg", 
                          predict.type = "prob",
                          par.vals = list(cost=16384), #change cost
                          fix.factors.prediction = TRUE)
#Wrapper to impute missing values
classif.lrn = makeImputeWrapper(classif.lrn, 
                                list(numeric = imputeMedian(),
                                     integer = imputeMedian(), 
                                     factor  = imputeMode()),
                                dummy.type = "numeric"
)


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

#compute benefits for varying costs of default
costs_l = as.list(1:20)
benef_logit = sapply(costs_l, function(x) comp_benef(costs = x, pred_test))
names(benef_logit) = 1:20


save(benef_logit, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_logit.Rdata")


