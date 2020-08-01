############IMPORTANT##################
# CAN TAKE SEVERAL HOURS TO COMPUTE
#########################################

setwd("H:/multivariate-methods/Code")
load("./Data_prep/train_val_data.Rdata")


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




#create task
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df_train_val, 
                               target = "Target_def" ) # specify train, validation


#specify learner
classif.lrn = makeLearner("classif.xgboost", 
                          par.vals = list(eval_metric ="auc"),
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)

### specify parameters to tune and values tried out
ps = makeParamSet(makeDiscreteParam("eta", values = c(0.01,0.025, 0.05)),
  makeDiscreteParam("nrounds", values = c(1000, 1500, 2000)),
  makeDiscreteParam("lambda", values = c(1, 1.5, 2)),
  makeDiscreteParam("subsample", values = c(0.6, 0.8, 1)),
  makeDiscreteParam("colsample_bytree", values = c(0.6, 0.8, 1)))


#optimization algo fo tuning --> simple grid tuning (taking all possible combinations of parameters)
ctrl = makeTuneControlGrid()

#specify cross validation
rdesc = makeResampleDesc("CV", iters = 4)

#parameter tuning
res = tuneParams(classif.lrn,
                 task = classif.task, 
                 resampling = rdesc,
                 measures = list(auc),
                 par.set = ps,
                 control = ctrl)

save(res, file = "XGBoost_tuning")


parallelStop()
