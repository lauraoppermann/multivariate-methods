setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")
load("./Code/Data_prep/train_val_data.Rdata")
library("mlr")
library("LiblineaR")
library("magrittr")
library("dplyr")
#library("caret")
library("parallelMap")
parallelStartSocket(4)


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

#specify cross validation with necessary steps:
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df_train_val, 
                               target = "Target_def")

classif.lrn = makeLearner("classif.LiblineaRL1LogReg", 
                          predict.type = "prob",
                          #par.vals = list(cost=100),
                          fix.factors.prediction = TRUE)
#standardize wrapper
classif.lrn = makePreprocWrapperCaret(classif.lrn, method = list(  Q_this_loan        = "norm",
                                                                   Q_loan_to_value    = "norm",        
                                                                   Q_Score            = "norm",            
                                                                   Q_Balance          = "norm",             
                                                                   Q_Month_Pay        = "norm",            
                                                                   Q_AVG_loan_Age     = "norm",        
                                                                   Q_Credit_hist_length  = "norm",       
                                                                   N_recent_loans       = "norm",      
                                                                   N_recent_defaults    = "norm",     
                                                                   N_Loans_Paid       = "norm",         
                                                                   Q_Loans_Paid_To_Tot     = "norm",    
                                                                   Q_Act_Debt_To_Total    = "norm",     
                                                                   Q_Loan_to_Balance      = "norm",     
                                                                   N_Loan_Applis          = "norm",      
                                                                   N_Age                 = "norm",      
                                                                   N_Loan_Date           = "norm",    
                                                                   N_Loans_Tot             = "norm",  
                                                                   N_Loans_Active           = "norm",   
                                                                   N_Loans_Overdue        = "norm",        
                                                                   C_State_id         = "norm",        
                                                                   C_brand_id         = "norm",          
                                                                   C_supplier_id      = "norm",        
                                                                   C_branch_id        = "norm",          
                                                                   C_Employee_id      = "norm",      
                                                                   C_Current_pincode_id     = "norm",   
                                                                   Q_this_loan1        = "norm",       
                                                                   Q_Balance1         = "norm",          
                                                                   Q_Month_Pay1      = "norm",           
                                                                   Q_AVG_loan_Age1    = "norm",         
                                                                   Q_Credit_hist_length1   = "norm",    
                                                                   N_recent_loans1      = "norm",        
                                                                   N_Loans_Paid1        = "norm",       
                                                                   Q_Loans_Paid_To_Tot1   = "norm",     
                                                                   Q_Act_Debt_To_Total1   = "norm",      
                                                                   Q_Loan_to_Balance1     = "norm",      
                                                                   N_Loan_Applis1          = "norm",   
                                                                   N_Age1                 = "norm",     
                                                                   N_Loans_Tot1           = "norm",     
                                                                   N_Loans_Active1        = "norm",     
                                                                   N_Loans_Overdue1        = "norm",    
                                                                   Q_this_loan_X_Q_loan_to_value = "norm",
                                                                   Q_this_loan_X_N_Age        = "norm",  
                                                                   Q_this_loan_X_Q_score       = "norm")  )   

#imputation wrapper
classif.lrn = makeImputeWrapper(classif.lrn, 
                                    list(numeric = imputeMedian(),
                                         integer = imputeMedian(), 
                                         factor  = imputeMode()),
                                    dummy.type = "numeric"
 )


#CV 
#rdesc = makeResampleDesc("CV", iters= 4)
#results = resample(classif.lrn, classif.task , rdesc, measures = list(auc),models = TRUE)


#optimization algo fo tuning --> simple grid tuning (taking all possible combinations of parameters)
ctrl = makeTuneControlGrid()

ps = makeParamSet(
  makeDiscreteParam("cost", values = c(0.5, 1,2, 4, 8, 16,32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768)
                    ))

#specify cross validation
rdesc = makeResampleDesc("CV", iters = 4)

#parameter tuning
res = tuneParams(classif.lrn,
                 task = classif.task, 
                 resampling = rdesc,
                 measures = list(auc),
                 par.set = ps,
                 control = ctrl)

save(res, file="./Code/1_Models/Logit_Tuning")
