setwd("C:/Users/felix/Documents/GitHub/multivariate-methods")
load("./Code/Data_prep/total_data.Rdata")


#install.packages("corrplot")
library("corrplot")
library("dplyr")

df$Target_def = as.numeric(df$Target_def)
corr_plot_data = df%>% select_if(is.numeric)
M = cor(corr_plot_data, use = "pairwise.complete.obs", method="pearson") 


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg('Correlogram.jpg',width = 600, height = 600)

corrplot(M, method="color", col=col(200),  
         type="upper", 
         #order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         #sig.level = 0.01, 
         #insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

dev.off()