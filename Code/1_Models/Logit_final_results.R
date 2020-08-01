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
  df_test,
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
save(mod, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/trained_model_logit.Rdata")
save(pred_test, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/predictions_logit.Rdata")




#############################################################################
#### COMPUTING AVG BENFITS  with optimal threshold strategy #################
#############################################################################
# we create functions that compute the average benefits of the decision strategy
# the variable cost represents the cost ratio (FALSE NEGATIVE to TRUE POSITIVE)
# --> 2 means it is two times more costly to lend to a band applciant than not to lend to a good one
# The costs are then calculated by taking the overall credit as the loss when giving a loan to a bad payer. 
# The opportunity costs of not giving to a good lender are given as: (1/cost)* loan amount
# We then calculate the average benefit (average over all credit applicatns)

######### varying weigths #####
comp_weight_benefit = function(df,pred,  costs){
  th = 1/(1+ costs)
  c_r = 1/costs
  pred = mlr::setThreshold(pred, th)
  cost_mat = cbind((as.numeric(df$Target_def)-1==0)*c_r*df$Q_this_loan -(as.numeric(df$Target_def)-1==1)*(df$Q_this_loan))
  costs = sum(cost_mat*((pred$data$truth== 1 & pred$data$response== 0) + (pred$data$truth== 0 & pred$data$response== 0)))
  return(costs / nrow(df))}

costs_l = as.list(1:20)
benef_logit = sapply(costs_l, function(x) comp_weight_benefit( df_test, pred_test,costs = x))
names(benef_logit) = 1:20

save(benef_logit, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_logit.Rdata")

#compute number of loans awarded:
n_loans_logit = lapply(1:20, function(x) sum(pred_test$data$prob.1 < (1/(1+x))))
save(n_loans_logit, file = "C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/n_loans_logit.Rdata")

