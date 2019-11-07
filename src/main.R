## ---- packages
library(tidyverse)
library(patchwork)
library(png)
library(tsfeatures)
library(RColorBrewer)
library(ggcorrplot) # to draw  ggcorrplot
library(M4comp2018)
library(GGally) # to draw scatterplot matrix


## ---- lengthboxplot
data(M4)
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)

length_Y <- data.frame(length=sapply(yearly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_Q <- data.frame(length=sapply(quarterly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_M <- data.frame(length=sapply(monthly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_W <- data.frame(length=sapply(weekly_M4, function(temp){length(c(temp$x, temp$xx))}))
length_D <- data.frame(length=sapply(daily_M4, function(temp){length(c(temp$x, temp$xx))}))
length_H <- sapply(hourly_M4, function(temp){length(c(temp$x, temp$xx))})

len_y <- ggplot(length_Y, aes(x = "length", y = length))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
    stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
      geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("yearly")
len_q <- ggplot(length_Q, aes(y = length, x = "quarterly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("quarterly")
len_m <- ggplot(length_Q, aes(y = length, x = "monthly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("monthly")
len_w <- ggplot(length_W, aes(y = length, x = "weekly"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("weekly")
len_d <- ggplot(length_D, aes(y = length, x = "daily"))+
  geom_point(position=position_jitter(width=0.1), color=alpha("red",0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.1, color="blue", size=1.25)+
  geom_boxplot(outlier.size=0, width=0.1)+theme(axis.title.x=element_blank(), axis.text.x=element_blank())+ggtitle("daily")
df <- data.frame(length=c("748", "1008"), frequency=c(169, 245))
len_h <- ggplot(df, aes(x=length, y=frequency)) +
  geom_bar(stat="identity")+ggtitle("hourly")
((len_y | len_q | len_m) / (len_w|len_d|len_h))

## ---- pca
# yearly
source("src/pcaprojection.R")
load("data/yearly/features_M4Y.rda")
load("data/yearly/yearly_features_training.rda") ## M1 and M3 data only
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc <- calculate_pca(yearly_features_training)
# vars_transformed <- apply(pca_ref_calc$prcomp_out$x, 2, var)
# vars_transformed/sum(vars_transformed)
ref_pca <- pca_ref_calc$pca_components
## Project M4 yearly data
load("data/yearly/features_M4Y.rda")
M4Y_PCA <- pca_projection(pca_ref_calc$prcomp_out, features_M4Y)
## draw PCA plot
pca_all_yearly <- bind_rows(ref_pca, M4Y_PCA,  .id="source")
pca_all_yearly$data <- c(rep("M1", 181),rep("M3",645),rep("Simulated", 10000), rep("M4", 23000))
pca_all_yearly_ref <- pca_all_yearly %>% filter(!data == "M4")
pcaY1  <- ggplot(pca_all_yearly_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly_ref, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly_ref, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Yearly: reference set")
pcaY2  <- ggplot(pca_all_yearly) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.001), shape=1)+
  geom_point(data=subset(pca_all_yearly, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.001))+
  geom_point(data=subset(pca_all_yearly, data=="M4"),aes(x=PC1, y=PC2, color=data), shape=1)+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863","#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Yearly M4 series (dark purple)")



#pcaY1 <- ggplot(pca_all_yearly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
#  scale_color_manual(values=c("forestgreen", NA))+theme(aspect.ratio = 1, axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
#  ggtitle("Yearly: reference set")+geom_point(data = pca_all_yearly[1:826, ], aes(x=PC1, y=PC2), colour="black", alpha=0.5)+
# theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
#pcaY2 <- ggplot(pca_all_yearly, aes(x=PC1, y=PC2, color=source)) + geom_point(alpha=0.2)+ theme(legend.position="none")+
#  scale_color_manual(values=c("forestgreen", "firebrick1"))+theme(aspect.ratio = 1, axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+ggtitle("Yearly M4 series (orange)")+
#  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
## Instance space colouring for yearly data
### Best forecast-model for yearly series
#yearly_training_best <- colnames(yearly_mase_fformpp)[max.col(yearly_mase_fformpp,ties.method="first")]
#load("data/yearly/predict_yearly_eval1000.rda")
#yearly_test_best <- colnames(predict_yearly_eval1000)[max.col(predict_yearly_eval1000,ties.method="first")]
#pca_all_yearly$best <- c(yearly_training_best, yearly_test_best)
### Yearly reference set
#M4Y_PCA$best <- yearly_test_best 
#pca_ref_best <- ggplot(M4Y_PCA, aes(x=PC1, y=PC2, color=best)) + geom_point()
### Yearly test set


# quarterly
load("data/quarterly/features_M4Q.rda")
load("data/quarterly/quarterly_features_training.rda") # M1 and M3 data only
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc_q <- calculate_pca(quarterly_features_training)
# vars_transformedq <- apply(pca_ref_calc_q$prcomp_out$x, 2, var)
# vars_transformedq/sum(vars_transformedq)
ref_pca_q <- pca_ref_calc_q$pca_components
## Project M4 quarterly data
load("data/quarterly/features_M4Q.rda")
M4Q_PCA <- pca_projection(pca_ref_calc_q$prcomp_out, features_M4Q)
## draw PCA plot
pca_all_quarterly <- bind_rows(ref_pca_q, M4Q_PCA,  .id="source")
pca_all_quarterly$data <- c(rep("M1", 203),rep("M3",756),rep("Simulated", 10000), rep("M4", 24000))
pca_all_quarterly_ref <- pca_all_quarterly %>% filter(!data == "M4")
pcaQ1  <- ggplot(pca_all_quarterly_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly_ref, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly_ref, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863","#e66101"))+xlab("PCA Component 1")+ylab("PCAComponent 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Quarterly: reference set")
pcaQ2  <- ggplot(pca_all_quarterly) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  geom_point(data=subset(pca_all_quarterly, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_quarterly, data=="M4"),aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863","#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Quarterly M4 series (dark purple)")


# monthly data
load("data/monthly/features_M4M.rda")
load("data/monthly/monthly_features_training.rda") #M1 and M3 data only
## Construct PCA space from simulated, M1 and M3 and then project M4-competition data
pca_ref_calc_m <- calculate_pca(monthly_features_training)
# vars_transformedm <- apply(pca_ref_calc_m$prcomp_out$x, 2, var)
# vars_transformedm/sum(vars_transformedm)
ref_pca_m <- pca_ref_calc_m$pca_components
## Project M4 yearly data
load("data/monthly/features_M4M.rda")
M4M_PCA <- pca_projection(pca_ref_calc_m$prcomp_out, features_M4M)
## draw PCA plot
pca_all_monthly <- bind_rows(ref_pca_m, M4M_PCA,  .id="source")
pca_all_monthly$data <- c(rep("M1", 617),rep("M3",1428),rep("Simulated", 10000), rep("M4", 48000))
pca_all_monthly_ref <- pca_all_monthly %>% filter(!data == "M4")
pcaM1  <- ggplot(pca_all_monthly_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly_ref, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly_ref, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863","#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Monthly: reference set")
pcaM2  <- ggplot(pca_all_monthly) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  geom_point(data=subset(pca_all_monthly, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly, data=="M3"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly, data=="M1"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_monthly, data=="M4"),aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#b2abd2","#fdb863","#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Monthly M4 series (dark purple)")


(pcaY1|pcaQ1|pcaM1)/(pcaY2|pcaQ2|pcaM2)

## ---- pca2
## Weekly data
load("data/weekly/features_M4W.rda")
load("data/weekly/features_weekly_marbased_x.rda") # Marbased features for weekly series
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_w <- calculate_pca(features_weekly_marbased_x)
ref_pca_w <- pca_ref_calc_w$pca_components
# vars_transformedw <- apply(pca_ref_calc_w$prcomp_out$x, 2, var)
# vars_transformedw/sum(vars_transformedw)
## Project M4 yearly data
load("data/weekly/features_M4W.rda")
M4W_PCA <- pca_projection(pca_ref_calc_w$prcomp_out, features_M4W)
## draw PCA plot
pca_all_weekly <- bind_rows(ref_pca_w, M4W_PCA,  .id="source")
pca_all_weekly$data <- c(rep("Simulated", 10000), rep("M4", 359))
pca_all_weekly_ref <- pca_all_weekly %>% filter(!data == "M4")
pcaW1  <- ggplot(pca_all_weekly_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_weekly_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Weekly: reference set")
pcaW2  <- ggplot(pca_all_weekly) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  geom_point(data=subset(pca_all_weekly, data=="M4"),aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  scale_alpha( guide="none")+
  scale_color_manual(values=c( "#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Weekly M4 series (dark purple)")

## daily data
load("data/daily/features_M4D.rda")
load("data/daily/features_daily_marbased.rda") # Mar-based series for daily series
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_d <- calculate_pca(features_daily_marbased)
# vars_transformedd <- apply(pca_ref_calc_d$prcomp_out$x, 2, var)
# vars_transformedd/sum(vars_transformedd)
ref_pca_d <- pca_ref_calc_d$pca_components
## Project M4 daily data
load("data/daily/features_M4D.rda")
M4D_PCA <- pca_projection(pca_ref_calc_d$prcomp_out, features_M4D)
## draw PCA plot
pca_all_daily <- bind_rows(ref_pca_d, M4D_PCA,  .id="source")
pca_all_daily$data <- c(rep("Simulated", 10000), rep("M4", 4227))
pca_all_daily_ref <- pca_all_daily %>% filter(!data == "M4")
pcaD1  <- ggplot(pca_all_daily_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_daily_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Daily: reference set")
pcaD2  <- ggplot(pca_all_daily) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  geom_point(data=subset(pca_all_daily, data=="M4"),aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c( "#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Daily M4 series (dark purple)")


## hourly data
load("data/hourly/features_M4H.rda")
load("data/hourly/features_hourly_marbased.rda") # Mar-based series for hourly series
## Construct PCA space from simulated and then project M4-competition data
pca_ref_calc_h <- calculate_pca(features_hourly_marbased)
# vars_transformedh <- apply(pca_ref_calc_h$prcomp_out$x, 2, var)
# vars_transformedh/sum(vars_transformedh)
ref_pca_h <- pca_ref_calc_h$pca_components
## Project M4 hourly data
load("data/hourly/features_M4H.rda")
M4H_PCA <- pca_projection(pca_ref_calc_h$prcomp_out, features_M4H)
## draw PCA plot
pca_all_hourly <- bind_rows(ref_pca_h, M4H_PCA,  .id="source")
pca_all_hourly$data <- c(rep("Simulated", 10000), rep("M4", 414))
pca_all_hourly_ref <- pca_all_hourly %>% filter(!data == "M4")
pcaH1  <- ggplot(pca_all_hourly_ref) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  geom_point(data=subset(pca_all_hourly_ref, data=="Simulated"),aes(x=PC1, y=PC2, color=data, alpha=0.01))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Hourly: reference set")
pcaH2  <- ggplot(pca_all_hourly) + geom_point(aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  geom_point(data=subset(pca_all_hourly, data=="M4"),aes(x=PC1, y=PC2, color=data, alpha=0.01), shape=1)+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c( "#5e3c99", "#e66101"))+xlab("PCA Component 1")+ylab("PCA Component 2")+
  theme(legend.title = element_blank()) +theme(aspect.ratio = 1)+
  ggtitle("Hourly M4 series (dark purple)")

(pcaW1|pcaD1|pcaH1)/(pcaW2|pcaD2|pcaH2)

## ---- prototypes
load("data/p1.rda")
p1 <- ggplot(p1, aes(x=X1, y=X2, color=Source)) +
  geom_point()+scale_color_manual(values=c( "#de2d26", "#1a1a1a"))+theme(aspect.ratio = 1)+ggtitle("A: Isolated clusters")+xlab("PCA component 1")+ylab("PCA component 2")
load("data/p2.rda")
p2 <- ggplot(p2, aes(x=X1, y=X2, color=Source)) +
  geom_point()+scale_color_manual(values=c( "#de2d26", "#1a1a1a"))+theme(aspect.ratio = 1)+ggtitle("B: Dense in a separate region")+xlab("PCA component 1")+ylab("PCA component 2")
p1|p2

## ---- corY
source("src/corrplot.R")
load("data/yearly/classlabelM1Y.rda")
clm1y <- classlabelM1Y$accuracy
mase_m1y <- clm1y[seq(1, nrow(clm1y), by = 2), ] 
## M3 competition yearly series
load("data/yearly/classlabelM3Y.rda") # on monash cluster
clm3y <- classlabelM3Y$accuracy
mase_m3y <- clm3y[seq(1, nrow(clm3y), by = 2), ] 
## MAR-based simulated time series
load("data/yearly/yearly_MARaccuracy.rda") # on monash cluster
## preparation of "Y" matrix
m1m3_mase <- rbind(mase_m1y, mase_m3y)
Y <- rbind(m1m3_mase, yearly_MARaccuracy)
Y <- data.frame(Y)
ggpairs(Y, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(Y,axisLabels="none")

## ---- corQ
## M1 competition quarterly series
load("data/quarterly/classlabelM1Q.rda") # on monash cluster
clm1q <- classlabelM1Q$accuracy
mase_m1q <- clm1q[seq(1, nrow(clm1q), by = 2), ][,-10] # remove mstl as it similar to ets
## M3 competition quarterly series
load("data/quarterly/classlabelM3Q.rda") 
clm3q <- classlabelM3Q$accuracy
mase_m3q <- clm3q[seq(1, nrow(clm3q), by = 2), ][,-10] # remove mstl as it similar to ets
## MAR-based simulated time series
load("data/quarterly/quarterly_MARaccuracy.rda") # on monash cluster
## preparation of "YQ" matrix
m1m3_mase <- rbind(mase_m1q, mase_m3q)
YQ <- rbind(m1m3_mase, quarterly_MARaccuracy)
YQ <- data.frame(YQ)
ggpairs(YQ, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(YQ, axisLabels="none")

## ---- corM
## Preparation of my data set for training
## M1 competition monthly series
load("data/monthly/classlabelM1M.rda") 
clm1m <- classlabelM1M$accuracy
mase_m1m <- clm1m[seq(1, nrow(clm1m), by = 2), ][,-10] # remove mstl as it similar to ets
## M3 competition monthly series
load("data/monthly/classlabelM3M.rda") 
clm3m <- classlabelM3M$accuracy
mase_m3m <- clm3m[seq(1, nrow(clm3m), by = 2), ][,-10] # remove mstl as it similar to ets
## MAR-based simulated time series
load("data/monthly/monthly_MARaccuracy.rda") 
m1m3_mase <- rbind(mase_m1m, mase_m3m)
YM <- rbind(m1m3_mase, monthly_MARaccuracy)
YM <- data.frame(YM)
ggpairs(YM, upper = list(continuous = corrplot),axisLabels="none")
#ggpairs(YM, axisLabels="none")

## ---- corW
load("data/weekly/weekly_mase_fformpp.rda")
weekly_mase_fformpp <- data.frame(weekly_mase_fformpp)
ggpairs(weekly_mase_fformpp, upper = list(continuous = corrplot),axisLabels="none")

## ---- corD
load("data/daily/daily_mase_fformpp.rda")
daily_mase_fformpp <- data.frame(daily_mase_fformpp)
ggpairs(daily_mase_fformpp, upper = list(continuous = corrplot),axisLabels="none")


## ---- corH
load("data/hourly/hourly_mase_fformpp.rda")
hourly_mase_fformpp <- data.frame(hourly_mase_fformpp)
ggpairs(hourly_mase_fformpp, upper = list(continuous = corrplot),axisLabels="none")

