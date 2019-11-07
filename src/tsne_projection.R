## ---- tsneplots
## Packages
library(Rtsne)
library(tidyverse)

## Yearly time series
# Load yearly training dataset
load("data/yearly/yearly_features_training.rda")
load("data/yearly/features_M4Y.rda")
## bind training data and features of M4 competition data into one column
yearly_tsne <- dplyr::bind_rows(yearly_features_training, features_M4Y)
yearly_tsne$source <- c(rep("M1", 181),rep("M3",645),rep("Simulated", 10000), rep("M4", 23000))
yearly_tsne_unique <- unique(yearly_tsne)
yearly_matrix <- as.matrix(yearly_tsne_unique[,1:25])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_yearly <- Rtsne(yearly_matrix) # Run TSNE
tsne_plot_yearly <- data.frame(x = tsne_out_yearly$Y[,1], y = tsne_out_yearly$Y[,2], col = yearly_tsne_unique$source)
tsne_plot_yearly$col <- factor(tsne_plot_yearly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsneY <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_yearly, col=="Simulated"),aes(x=x, y=y, color=col, alpha=0.01))+
  geom_point(data=subset(tsne_plot_yearly, col=="M1"),aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_yearly, col=="M3"),aes(x=x, y=y, color=col))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99","#b2abd2", "#fdb863"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Yearly")
  


## Quarterly time series
# Load quarterly training dataset
load("data/quarterly/quarterly_features_training.rda")
load("data/quarterly/features_M4Q.rda")
## bind training data and features of M4 competition data into one column
quarterly_tsne <- dplyr::bind_rows(quarterly_features_training, features_M4Q)
quarterly_tsne$source <- c(rep("M1", 203),rep("M3",756),rep("Simulated", 10000), rep("M4", 24000))
quarterly_tsne_unique <- unique(quarterly_tsne)
quarterly_matrix <- as.matrix(quarterly_tsne_unique[,1:30])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_quarterly <- Rtsne(quarterly_matrix) # Run TSNE
tsne_plot_quarterly <- data.frame(x = tsne_out_quarterly$Y[,1], y = tsne_out_quarterly$Y[,2], col = quarterly_tsne_unique$source)
tsne_plot_quarterly$col <- factor(tsne_plot_quarterly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsneQ <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_quarterly, col=="Simulated"),aes(x=x, y=y, color=col, alpha=0.01))+
  geom_point(data=subset(tsne_plot_quarterly, col=="M1"),aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_quarterly, col=="M3"),aes(x=x, y=y, color=col))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99","#b2abd2", "#fdb863"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Quarterly")

## Monthly time series
load("data/monthly/monthly_features_training.rda")
load("data/monthly/features_M4M.rda")
## bind training data and features of M4 competition data into one column
monthly_tsne <- dplyr::bind_rows(monthly_features_training, features_M4M)
monthly_tsne$source <- c(rep("M1", 617),rep("M3",1428),rep("Simulated", 10000), rep("M4", 48000))
monthly_tsne_unique <- unique(monthly_tsne)
monthly_matrix <- as.matrix(monthly_tsne_unique[,1:30])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_monthly <- Rtsne(monthly_matrix, check_duplicates = FALSE) # Run TSNE
tsne_plot_monthly <- data.frame(x = tsne_out_monthly$Y[,1], y = tsne_out_monthly$Y[,2], col = monthly_tsne_unique$source)
tsne_plot_monthly$col <- factor(tsne_plot_monthly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsneM <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_monthly, col=="Simulated"),aes(x=x, y=y, color=col, alpha=0.01))+
  geom_point(data=subset(tsne_plot_monthly, col=="M1"),aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_monthly, col=="M3"),aes(x=x, y=y, color=col))+scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99","#b2abd2", "#fdb863"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Monthly")

## Weekly time series
load("data/weekly/features_weekly_marbased_x.rda")
load("data/weekly/features_M4W.rda")
## bind training data and features of M4 competition data into one column
weekly_tsne <- dplyr::bind_rows(features_weekly_marbased_x, features_M4W)
weekly_tsne$source <- c(rep("Simulated", 10000), rep("M4", 359))
weekly_tsne_unique <- unique(weekly_tsne)
weekly_matrix <- as.matrix(weekly_tsne_unique[,1:27])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_weekly <- Rtsne(weekly_matrix) # Run TSNE
tsne_plot_weekly <- data.frame(x = tsne_out_weekly$Y[,1], y = tsne_out_weekly$Y[,2], col = weekly_tsne_unique$source)
tsne_plot_weekly$col <- factor(tsne_plot_weekly$col, levels = c("Simulated", "M4"))
tsneW <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=col))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Weekly")

## Dailytime series
load("data/daily/features_daily_marbased.rda")
load("data/daily/features_M4D.rda")
## bind training data and features of M4 competition data into one column
daily_tsne <- dplyr::bind_rows(features_daily_marbased, features_M4D)
daily_tsne$source <- c(rep("Simulated", 10000), rep("M4", 4227))
daily_tsne_unique <- unique(daily_tsne)
daily_matrix <- as.matrix(daily_tsne_unique[,1:26])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_daily<- Rtsne(daily_matrix) # Run TSNE
tsne_plot_daily <- data.frame(x = tsne_out_daily$Y[,1], y = tsne_out_daily$Y[,2], col = daily_tsne_unique$source)
tsne_plot_daily$col <- factor(tsne_plot_daily$col, levels = c("Simulated", "M4"))
tsneD <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=col))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Daily")

## Hourlytime series
load("data/hourly/features_hourly_marbased.rda")
load("data/hourly/features_M4H.rda")
## bind training data and features of M4 competition data into one column
hourly_tsne <- dplyr::bind_rows(features_hourly_marbased, features_M4H)
hourly_tsne$source <- c(rep("Simulated", 10000), rep("M4", 414))
hourly_tsne_unique <- unique(hourly_tsne)
hourly_matrix <- as.matrix(hourly_tsne_unique[,1:26])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_hourly <- Rtsne(hourly_matrix) # Run TSNE
tsne_plot_hourly <- data.frame(x = tsne_out_hourly$Y[,1], y = tsne_out_hourly$Y[,2], col = hourly_tsne_unique$source)
tsne_plot_hourly$col <- factor(tsne_plot_hourly$col, levels = c("Simulated", "M4"))
tsneH <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y, color=col))+
  geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=col))+
  scale_alpha(guide = 'none')+
  scale_color_manual(values=c("#e66101","#5e3c99"))+xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+
  theme(legend.title = element_blank()) + ggtitle("Hourly")


(tsneY|tsneQ|tsneM)/(tsneW|tsneD|tsneH)

## ---- coverage
source("src/calculateMisCoverage.R") ## General function to compute miscoverage
source("src/hist2d.R")
## Yearly-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_yearly, data1="M4", data2="Simulated", nbins = 30) # 0.02666667
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_yearly, data1="Simulated", data2="M4", nbins = 30) # 0.06888889

## Quarterly-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_quarterly, data1="M4", data2="Simulated", nbins = 30) # 0.02222222
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_quarterly, data1="Simulated", data2="M4", nbins = 30) # 0.04333333

## Monthly-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_monthly, data1="M4", data2="Simulated", nbins = 30) # 0.01111111
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_monthly, data1="Simulated", data2="M4", nbins = 30) #  0.1322222

## Weekly-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_weekly, data1="M4", data2="Simulated", nbins = 30) #0.4388889
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_weekly, data1="Simulated", data2="M4", nbins = 30) #0.001111111

## Daily-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_daily, data1="M4", data2="Simulated", nbins = 30) #0.2777778
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_daily, data1="Simulated", data2="M4", nbins = 30) # 0.06111111

## Hourly-------
### The miscoverage of dataset "M4" over dataset "Reference set" is defined as
calculateMisCoverage(tsne_plot_hourly, data1="M4", data2="Simulated", nbins = 30) #0.49
### The miscoverage of dataset "Reference set" over dataset "M4" is defined as
calculateMisCoverage(tsne_plot_hourly, data1="Simulated", data2="M4", nbins = 30)# 0.011
