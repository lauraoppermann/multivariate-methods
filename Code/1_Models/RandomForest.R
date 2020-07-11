setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")
load("./Code/Data_prep/preproc_data.Rdata")
library("mlr")
library("LiblineaR")
library("magrittr")
library("dplyr")
#library("caret")

#data preprocess specific to logit
#add squared variables
df %<>%
  mutate(
    disbursed_amount2 = disbursed_amount^2,
    asset_cost2 = asset_cost^2,
    ltv2 = ltv^2,
    AVERAGE.ACCT.AGE2 = AVERAGE.ACCT.AGE^2,
    CREDIT.HISTORY.LENGTH2 = CREDIT.HISTORY.LENGTH^2
  )

#change factor to dummy
df = createDummyFeatures(
  df,
  target = "loan_default",
  method = "1-of-n",
  cols = NULL
)


#specify cross validation with necessary steps:
classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df, 
                               target = "loan_default")

classif.lrn = makeLearner("classif.LiblineaRL1LogReg", 
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)
#standardize wrapper
classif.lrn = makePreprocWrapperCaret(classif.lrn, method = list( disbursed_amount = "norm"))

#imputation wrapper
classif.lrn = makeImputeWrapper(classif.lrn, 
                                    list(numeric = imputeMedian(),
                                         integer = imputeMedian(), 
                                         factor  = imputeMode()),
                                    dummy.type = "numeric"
 )


#CV 
rdesc = makeResampleDesc("CV", iters = 2, stratify=TRUE)
resample(classif.lrn, classif.task , rdesc, measures = list(auc),models = TRUE)


#
mod = train(classif.lrn, task = classif.task )
pred = predict(mod, task = classif.task)
cal = generateCalibrationData(pred,breaks = c(0, seq(from= 0.5, to =1, by = 0.05)))

plotCalibration(cal, rag=FALSE)
hist(pred$data$prob.1)

#plot roc curve
Roc_data = generateThreshVsPerfData(pred, measures = list(fpr, tpr, mmce))
plotROCCurves(Roc_data)
calculateROCMeasures(pred)
