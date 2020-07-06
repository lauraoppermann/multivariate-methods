setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")
load("./Code/Data_prep/preproc_data.Rdata")
library("mlr")
library("penalized")
library("LiblineaR")

df = createDummyFeatures(
  df,
  target = "loan_default",
  method = "1-of-n",
  cols = NULL
)

classif.task = makeClassifTask(id = "CreditScoring", 
                               data = df, 
                               target = "loan_default")

classif.lrn = makeLearner("classif.LiblineaRL1LogReg", 
                          predict.type = "prob", 
                          fix.factors.prediction = TRUE)

classif.lrn.imp = makeImputeWrapper(classif.lrn, 
                                    list(numeric = imputeMedian(),
                                         integer = imputeMedian(), 
                                         factor =imputeMode()),
                                    dummy.type = "numeric"
 )

rdesc = makeResampleDesc("CV", iters = 5, stratify=TRUE)
resample(classif.lrn.imp, classif.task , rdesc, measures = list(auc),models = TRUE)