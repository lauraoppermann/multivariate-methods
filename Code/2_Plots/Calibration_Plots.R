library("ggplot2")
library("grid")

###Plot 1: 

load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/predictions_xgboost.Rdata")
xgboost_pred = pred_test

load("C:/Users/felix/Documents/GitHub/multivariate-methods/Code/1_Models/predictions_logit.Rdata")
logit_pred = pred_test

##xgboost

#compute performance on test 
mlr::performance(xgboost_pred, mlr::auc)

#compute calibration
cal = generateCalibrationData(xgboost_pred, breaks = seq(from= 0.0, to =1, by = 0.05))
calib_xgboost = plotCalibration(cal, rag=FALSE, reference=FALSE) + 
  geom_segment(aes(x = 1, y = 0.025, xend = 20, yend = 0.975),  color="black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.title = element_blank(),  legend.position = "none")+
  scale_color_manual(values=c("#FC4E07"))+
  scale_y_continuous(breaks = seq(0, 1, 0.05))

#compute histogram
hist_xgboost = ggplot(xgboost_pred$data, aes(x=prob.1)) + 
  geom_histogram( breaks=seq(0,1,0.05)) +
  theme_bw() + 
  theme(legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        #axis.text.y=element_blank(),
        panel.grid.minor = element_blank()
        ) +
  labs(y = "Count", title = "xgboost") +
  coord_fixed(ratio=0.00002) +
  scale_x_continuous(breaks = seq(0, 1, 0.05))

##logit

#compute performance on test 
mlr::performance(logit_pred, mlr::auc)

#compute calibration
cal = generateCalibrationData(logit_pred, breaks = seq(from= 0.0, to =1, by = 0.05))
calib_logit = plotCalibration(cal, rag=FALSE, reference=FALSE) + 
  geom_segment(aes(x = 1, y = 0.025, xend = 20, yend = 0.975),  color="black") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")+
  scale_color_manual(values=c("#FC4E07"))+
  scale_y_continuous(breaks = seq(0, 1, 0.05))

#compute histogram
hist_logit = ggplot(logit_pred$data, aes(x=prob.1)) + 
  geom_histogram(breaks = seq(0,1, 0.05))+
    #breaks=seq(0,1,0.05), boundary = -0.025) +
  theme_bw() + 
  theme(legend.title = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "logit") +
  coord_fixed(ratio=0.00002) +
  scale_x_continuous(breaks = seq(0.025, 0.975, 0.05),   limits = c(0, 1), expand = c(0, 0)) 


#########################################################################

hist_logit = ggplot(logit_pred$data, aes(x=prob.1)) + 
  stat_bin(breaks = seq(0,1, 0.05), center = TRUE)+
  #breaks=seq(0,1,0.05), boundary = -0.025) +
  theme_bw() + 
  theme(legend.title = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "logit") +
  coord_fixed(ratio=0.00002) +
  scale_x_continuous(breaks = seq(0, 1, 0.05),   limits = c(0, 1) )#, expand = c(0, 0)) 



grid.newpage()
grid.draw(cbind(rbind(ggplotGrob(hist_xgboost),   ggplotGrob(calib_xgboost)), rbind(ggplotGrob(hist_logit),   ggplotGrob(calib_logit)), size = "first"))







