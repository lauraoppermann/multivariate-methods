library("ggplot2")
library("grid")

###Plot 1: 

load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_xgboost.Rdata")
load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_logit.Rdata")
load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/benef_naive.Rdata")

#benef_logit = benef_logit[3:20]
#benef_xgboost = benef_xgboost[3:20]
#benef_naive = benef_naive[3:20]

benef = as.data.frame(c(t(benef_xgboost), t(benef_logit), t(benef_naive)))
benef = cbind(benef, as.data.frame(rep(1:20, 3)), as.data.frame(c(rep("xgboost", 20), rep("logit", 20), rep("all_approved",20 ))))
names(benef) = c("TotalProfits", "Cost_to_Benefit_ratio", "model")

profits = ggplot(data=benef, aes(Cost_to_Benefit_ratio, TotalProfits, color=factor(model, levels = c("xgboost", "logit", "all_approved")))) + 
  geom_line() + 
  geom_point() + 
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.title = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Average Profit per Applicant [INR]") +
  scale_color_manual(values=c("#FC4E07","#00AFBB", "#999999")) +
  scale_x_continuous(breaks = seq(1, 20, 1)) +
  scale_y_continuous(breaks = seq(-10000, 30000, 2500))

  #+coord_trans( y="log10")

  


###Plot 2: 
Profit_Ratio = as.data.frame(benef_xgboost / benef_logit)
Cost_to_Benefit_ratio = as.data.frame(1:20)
Profit_Ratio= cbind(Profit_Ratio, Cost_to_Benefit_ratio)
names(Profit_Ratio) = c("Profit_Ratio", "Cost_to_Benefit_ratio")


profit_ratio = ggplot(data=Profit_Ratio, aes(Cost_to_Benefit_ratio, Profit_Ratio, color= "xgboost-to-logit" )) + 
  geom_line() +
  geom_point() + 
  theme_bw() + 
  theme(legend.title = element_blank(), axis.title.x=element_text(), axis.text.x=element_text(), panel.grid.minor = element_blank()) +
  labs(x = "Cost-to-Benefit Ratio", y = "Profit-Ratio") +
  coord_fixed(ratio=2) + 
  scale_color_manual(values=c("black"))+
  scale_x_continuous(breaks = seq(1, 20, 1)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) 

#  +coord_trans( y="log10")


###combine plots
grid.newpage()
grid.draw(rbind(ggplotGrob(profits),   ggplotGrob( profit_ratio),  size = "first"))

