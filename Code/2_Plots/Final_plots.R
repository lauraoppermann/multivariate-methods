load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_xgboost.Rdata")
load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_logit.Rdata")
load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_naive.Rdata")

benef = rbind(benef_xgboost, benef_logit, benef_naive )
colnames(benef) = 1:20