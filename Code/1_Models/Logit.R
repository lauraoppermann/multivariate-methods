# just some first implementation, more copy and paste

setwd("C:/Users/Laura/Documents/Uni/MV/multivariate-methods")
load("./Code/Data_prep/preproc_data.Rdata")
library("mlr")
library("penalized")
library("stats")


df = createDummyFeatures(
  df,
  target = "loan_default",
  method = "1-of-n",
  cols = NULL
)


classif.task = makeClassifTask(id = "CreditScoringLogit", 
                               data = df, 
                               target = "loan_default")

classif.lrn = makeLearner("classif.logreg", 
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)

classif.lrn.imp = makeImputeWrapper(classif.lrn, 
                                    list(numeric = imputeMedian(),
                                         integer = imputeMedian(), 
                                         factor =imputeMode()),
                                    dummy.type = "numeric"
 )

rdesc = makeResampleDesc("CV", iters = 3, stratify=TRUE)
result = resample(classif.lrn.imp, classif.task , rdesc, measures = list(auc),models = TRUE)
result
