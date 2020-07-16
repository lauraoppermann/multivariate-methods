setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")
load("./Code/Data_prep/train_val_test_data.Rdata")


library("mlr")
library("magrittr")
library("dplyr")
library("randomForestSRC")

library("parallelMap")
parallelStartSocket(4)


#library("caret")



#change factor to dummy
#df = createDummyFeatures(
#  df,
#  target = "loan_default",
#  method = "1-of-n",
#  cols = NULL
#)




#specify cross validation with necessary steps:
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = train_val_data, 
                               target = "Target_def" ,
                               blocking = train_val_data$Block) # specify train, validation

#parameter space
ps = makeParamSet(
  makeDiscreteParam("mtry", values = c(4,8,12)),
  makeDiscreteParam("nsplit", values = c(10,20,30)),
  makeDiscreteParam("sampsize", values = c(0.6,0.7,0.8)),
  makeDiscreteParam("nodesize", values = c(1,50,200)),
)

#optimization algo fo tuning
ctrl = makeTuneControlGrid()

#learner
classif.lrn = makeLearner("classif.randomForestSRC", 
                          par.vals = list(samptype="swr", ntree =100),
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)

#standardize wrapper
#classif.lrn = makePreprocWrapperCaret(classif.lrn, method = list( disbursed_amount = "norm"))

#imputation wrapper
#classif.lrn = makeImputeWrapper(classif.lrn, 
#                                    list(numeric = imputeMedian(),
#                                         integer = imputeMedian(), 
#                                         factor  = imputeMode()),
#                                          dummy.type = "numeric")


#CV 
rdesc = makeResampleDesc("Holdout", blocking.cv = TRUE, iter = 2)
results = resample(classif.lrn, classif.task , rdesc, measures = list(auc),models = TRUE)

res = tuneParams(classif.lrn, task = classif.task, resampling = rdesc, par.set = ps,
                 control = ctrl, measures = list(auc), show.info = TRUE)


####################################################################################################
mod = train(classif.lrn, task = classif.task )
pred_test = predict(mod, makeClassifTask(data = test,  target = "Target_def"))
pred_train = predict(mod, makeClassifTask(data = train,  target = "Target_def"))
mlr::performance(pred_train, mlr::auc)
mlr::performance(pred_test, mlr::auc)
cal = generateCalibrationData(pred,breaks = c(0, seq(from= 0.5, to =1, by = 0.05)))

plotCalibration(cal, rag=FALSE)
hist(pred$data$prob.1)

#plot roc curve
Roc_data = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
plotROCCurves(Roc_data)
calculateROCMeasures(pred)
