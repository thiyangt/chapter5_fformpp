## Packages
library(tidyverse)
library(colorspace)
#devtools::install_github("zeehio/facetscales")
library(facetscales)

## Prediction interval calculations----------------------------------------------
## Function to return names of the models used for combination
combined_models <- function(predicted, ncomp=4, forecast_list, nseries, h){
  
  tpredicted <- t(predicted)
  pred.list <- lapply(seq_len(ncol(tpredicted)), function(i) tpredicted[,i])
  fm <- lapply(pred.list, function(temp, ncomp) {
    which(temp %in% sort(unique(temp))[1:ncomp])
  }, ncomp=ncomp)
  

  all_models <- list()
  
  for (i in 1: length(fm)){
    
    all_models[[i]] <- colnames(predicted)[fm[[i]]]

    
  }
  
  list(allModels=all_models)
}

## Yearly series
load("data/yearly/monash_cluster_model_output/predict_yearly_eval500.rda")
load("data/yearly/yearlyM4_forecasts.rda")
models_yearlyM4 <- combined_models(predict_yearly_eval500, ncomp=4, 
                                         yearlyM4_forecasts,
                                         nseries=23000,
                                         h=6)

allY <- data.frame(table(unlist(models_yearlyM4))/23000*100)
colnames(allY) <- c("forecast-model", "percentage")
# arima      ets       nn       rw 
# 82.71304 80.40000 34.59565 33.71739 
# rwd    theta       wn 
# 79.11739 79.22609 10.23043

arima <- rep(0,23000)
ets <- rep(0,23000)
nn <- rep(0,23000)
rw <- rep(0,23000)
rwd <- rep(0,23000)
theta <- rep(0,23000)
wn <- rep(0,23000)
for(i in 1:23000){

  if (c("arima") %in% models_yearlyM4[[1]][[i]]){arima[i]=1}
  if (c("ets") %in% models_yearlyM4[[1]][[i]]){ets[i]=1}
  if (c("nn") %in% models_yearlyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_yearlyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_yearlyM4[[1]][[i]]){rwd[i]=1}
  if (c("theta") %in% models_yearlyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_yearlyM4[[1]][[i]]){wn[i]=1}
}

yearly_cluster <- data.frame(arima=arima, ets=ets, nn=nn, rw=rw, rwd=rwd, theta=theta, wn=wn)
#library(vegan)
dist_yearly <- dist(yearly_cluster, method = "binary")
#dist_yearly <- vegdist(yearly_cluster, method = "jaccard")
hc_yearly <- hclust(dist_yearly, method="ward")
yearly_clust <- cutree(hc_yearly, k = 3)
yearly_cluster$yearly_clusters <- yearly_clust
save(yearly_clust, file="data/yearly/yearly_clust.rda")
table(yearly_clust)
load("data/yearly/yearly_clust.rda")
cl1y <- dplyr::filter(yearly_cluster, yearly_clusters==1) %>% select(c(-yearly_clusters))
cl2y <- dplyr::filter(yearly_cluster, yearly_clusters==2) %>% select(c(-yearly_clusters))
cl3y <- dplyr::filter(yearly_cluster, yearly_clusters==3) %>% select(c(-yearly_clusters))

cl1y_df <- as.data.frame(as.table(colSums(cl1y)/nrow(cl1y)*100))
colnames(cl1y_df) <- c("forecast-model", "percentage")
cl2y_df <- as.data.frame(as.table(colSums(cl2y)/nrow(cl2y)*100))
colnames(cl2y_df) <- c("forecast-model", "percentage")
cl3y_df <- as.data.frame(as.table(colSums(cl3y)/nrow(cl3y)*100))
colnames(cl3y_df) <- c("forecast-model", "percentage")
cl1y_df
cl2y_df
cl3y_df

Yearly_clust_df <- do.call("rbind", list(allY, cl1y_df, cl2y_df, cl3y_df))
Yearly_clust_df$source <- rep(c("all series", "Cluster 2", "Cluster 1", "Cluster 3"), each=7)
Yearly_clust_df$percentage <- round(Yearly_clust_df$percentage,2)
Yearly_clust_df

## Quarterly series######################################################################
load("data/quarterly/monash_cluster_model_outputQ/predict_quarterly_eval500.rda")
load("data/quarterly/quarterlyM4_forecast.rda")
models_quarterlyM4 <- combined_models(predict_quarterly_eval500, ncomp=4, 
                                      quarterlyM4_forecast,
                                      nseries=24000,
                                      h=8)
table(unlist(models_quarterlyM4))/24000*100
allQ <- data.frame(table(unlist(models_quarterlyM4))/24000*100)
colnames(allQ) <- c("forecast-model", "percentage")
# arima       ets        nn        rw 
# 93.054167 82.808333 35.837500 11.400000 
# rwd    snaive     stlar     tbats 
# 47.833333  9.054167 12.987500 87.400000 
# theta        wn 
# 13.841667  5.783333



arima <- rep(0,24000)
ets <- rep(0,24000)
nn <- rep(0,24000)
rw <- rep(0,24000)
rwd <- rep(0,24000)
snaive <- rep(0, 24000)
stlar <- rep(0, 24000)
tbats <- rep(0, 24000)
theta <- rep(0,24000)
wn <- rep(0,24000)
for(i in 1:24000){
  
  if (c("arima") %in% models_quarterlyM4[[1]][[i]]){arima[i]=1}
  if (c("ets") %in% models_quarterlyM4[[1]][[i]]){ets[i]=1}
  if (c("nn") %in% models_quarterlyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_quarterlyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_quarterlyM4[[1]][[i]]){rwd[i]=1}
  if (c("snaive") %in% models_quarterlyM4[[1]][[i]]){snaive[i]=1}
  if (c("stlar") %in% models_quarterlyM4[[1]][[i]]){stlar[i]=1}
  if (c("tbats") %in% models_quarterlyM4[[1]][[i]]){tbats[i]=1}
  if (c("theta") %in% models_quarterlyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_quarterlyM4[[1]][[i]]){wn[i]=1}
}

quarterly_cluster <- data.frame(arima=arima, ets=ets, nn=nn, rw=rw, rwd=rwd, snaive=snaive, stlar=stlar, tbats=tbats, theta=theta, wn=wn)
dist_quarterly<- dist(quarterly_cluster, method = "binary")
hc_quarterly <- hclust(dist_quarterly, method="ward")
quarterly_clust <- cutree(hc_quarterly, k = 3)
save(quarterly_clust, file="data/quarterly/quarterly_clust.rda")
table(quarterly_clust)
load("data/quarterly/quarterly_clust.rda")
quarterly_cluster$quarterly_clust <- quarterly_clust
cl1q <- dplyr::filter(quarterly_cluster, quarterly_clust==1) %>% select(c(-quarterly_clust))
cl2q <- dplyr::filter(quarterly_cluster, quarterly_clust==2) %>% select(c(-quarterly_clust))
cl3q <- dplyr::filter(quarterly_cluster, quarterly_clust==3) %>% select(c(-quarterly_clust))

cl1q_df <- as.data.frame(as.table(colSums(cl1q)/nrow(cl1q)*100))
colnames(cl1q_df) <- c("forecast-model", "percentage")
cl2q_df <- as.data.frame(as.table(colSums(cl2q)/nrow(cl2q)*100))
colnames(cl2q_df) <- c("forecast-model", "percentage")
cl3q_df <- as.data.frame(as.table(colSums(cl3q)/nrow(cl3q)*100))
colnames(cl3q_df) <- c("forecast-model", "percentage")

Quarterly_clust_df <- do.call("rbind", list(allQ, cl1q_df, cl2q_df, cl3q_df))
Quarterly_clust_df$source <- rep(c("all series", "Cluster 1", "Cluster 2", "Cluster 3"), each=10)
Quarterly_clust_df$percentage <- round(Quarterly_clust_df$percentage,2)
Quarterly_clust_df


## Monthly Series #######################################################################
load("data/monthly/monash_cluster_model_outputM/predict_monthly_eval500.rda")
load("data/monthly/monthlyM4_forecast.rda")
models_monthlyM4 <- combined_models(predict_monthly_eval500, ncomp=4, 
                                    monthlyM4_forecast,
                                    nseries=48000,
                                    h=18)
table(unlist(models_monthlyM4))/48000*100
allM <- data.frame(table(unlist(models_monthlyM4))/48000*100)
colnames(allM) <- c("forecast-model", "percentage")
# arima       ets        nn        rw 
# 88.997917 74.402083 31.785417 16.075000 
# rwd    snaive     stlar     tbats 
# 45.345833  7.056250 19.891667 83.058333 
# theta        wn 
# 30.633333  2.754167


arima <- rep(0,48000)
ets <- rep(0,48000)
nn <- rep(0,48000)
rw <- rep(0,48000)
rwd <- rep(0,48000)
snaive <- rep(0, 48000)
stlar <- rep(0, 48000)
tbats <- rep(0, 48000)
theta <- rep(0,48000)
wn <- rep(0,48000)
for(i in 1:48000){
  
  if (c("arima") %in% models_monthlyM4[[1]][[i]]){arima[i]=1}
  if (c("ets") %in% models_monthlyM4[[1]][[i]]){ets[i]=1}
  if (c("nn") %in% models_monthlyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_monthlyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_monthlyM4[[1]][[i]]){rwd[i]=1}
  if (c("snaive") %in% models_monthlyM4[[1]][[i]]){snaive[i]=1}
  if (c("stlar") %in% models_monthlyM4[[1]][[i]]){stlar[i]=1}
  if (c("tbats") %in% models_monthlyM4[[1]][[i]]){tbats[i]=1}
  if (c("theta") %in% models_monthlyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_monthlyM4[[1]][[i]]){wn[i]=1}
}

monthly_cluster_full <- data.frame(arima=arima, ets=ets, nn=nn, rw=rw, rwd=rwd, snaive=snaive, stlar=stlar, tbats=tbats, theta=theta, wn=wn)
set.seed(22072019)
sample_id <- sample(nrow(monthly_cluster_full),30000)
save(sample_id, file="data/monthly/sample_id.rda")
load("data/monthly/sample_id.rda")
monthly_cluster <- monthly_cluster_full[sample_id, ]
dist_monthly <- dist(monthly_cluster, method = "binary")
hc_monthly <- hclust(dist_monthly, method="ward")
monthly_clust <- cutree(hc_monthly, k = 3)
save(monthly_clust, file="data/monthly/monthly_clust.rda")
load("data/monthly/monthly_clust.rda")
table(monthly_clust)
monthly_cluster$monthly_clust <- monthly_clust
cl1m <- dplyr::filter(monthly_cluster, monthly_clust==1) %>% select(c(-monthly_clust))
cl2m <- dplyr::filter(monthly_cluster, monthly_clust==2) %>% select(c(-monthly_clust))
cl3m <- dplyr::filter(monthly_cluster, monthly_clust==3) %>% select(c(-monthly_clust))

cl1m_df <- as.data.frame(as.table(colSums(cl1m)/nrow(cl1m)*100))
colnames(cl1m_df) <- c("forecast-model", "percentage")
cl2m_df <- as.data.frame(as.table(colSums(cl2m)/nrow(cl2m)*100))
colnames(cl2m_df) <- c("forecast-model", "percentage")
cl3m_df <- as.data.frame(as.table(colSums(cl3m)/nrow(cl3m)*100))
colnames(cl3m_df) <- c("forecast-model", "percentage")

Monthly_clust_df <- do.call("rbind", list(allM, cl1m_df, cl2m_df, cl3m_df))
Monthly_clust_df$source <- rep(c("all series", "Cluster 2", "Cluster 1", "Cluster 3"), each=10)
Monthly_clust_df$percentage <- round(Monthly_clust_df$percentage,2)
Monthly_clust_df



## Weekly series
load("data/weekly/monash_cluster_model_outputW/predict_weekly_eval500.rda")
load("data/weekly/weeklyM4_forecast.rda")
models_weeklyM4 <- combined_models(predict_weekly_eval500, ncomp=4, 
                                   weeklyM4_forecast,
                                   nseries=359,
                                   h=13)
table(unlist(models_weeklyM4))/359*100
allW <- data.frame(table(unlist(models_weeklyM4))/359*100)
colnames(allW) <- c("forecast-model", "percentage")

# arima        nn        rw       rwd    snaive     stlar     tbats     theta        wn 
# 94.428969 31.476323  9.749304 69.359331 10.863510 29.526462 75.487465 74.651811  4.456825 


arima <- rep(0,359)
nn <- rep(0,359)
rw <- rep(0,359)
rwd <- rep(0,359)
snaive <- rep(0, 359)
stlar <- rep(0, 359)
tbats <- rep(0, 359)
theta <- rep(0,359)
wn <- rep(0,359)
for(i in 1:359){
  
  if (c("arima") %in% models_weeklyM4[[1]][[i]]){arima[i]=1}
  if (c("nn") %in% models_weeklyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_weeklyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_weeklyM4[[1]][[i]]){rwd[i]=1}
  if (c("snaive") %in% models_weeklyM4[[1]][[i]]){snaive[i]=1}
  if (c("stlar") %in% models_weeklyM4[[1]][[i]]){stlar[i]=1}
  if (c("tbats") %in% models_weeklyM4[[1]][[i]]){tbats[i]=1}
  if (c("theta") %in% models_weeklyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_weeklyM4[[1]][[i]]){wn[i]=1}
}

weekly_cluster <- data.frame(arima=arima, nn=nn, rw=rw, rwd=rwd, snaive=snaive, stlar=stlar, tbats=tbats, theta=theta, wn=wn)
dist_weekly <- dist(weekly_cluster, method = "binary")
hc_weekly <- hclust(dist_weekly, method="ward")
weekly_clust <- cutree(hc_weekly, k = 3)
table(weekly_clust)
save(weekly_clust, file="data/weekly/weekly_clust.rda")
load("data/weekly/weekly_clust.rda")
weekly_cluster$weekly_clust <- weekly_clust

cl1w <- dplyr::filter(weekly_cluster, weekly_clust==1) %>% select(c(-weekly_clust))
cl2w <- dplyr::filter(weekly_cluster, weekly_clust==2) %>% select(c(-weekly_clust))
cl3w <- dplyr::filter(weekly_cluster, weekly_clust==3) %>% select(c(-weekly_clust))

cl1w_df <- as.data.frame(as.table(colSums(cl1w)/nrow(cl1w)*100))
colnames(cl1w_df) <- c("forecast-model", "percentage")
cl2w_df <- as.data.frame(as.table(colSums(cl2w)/nrow(cl2w)*100))
colnames(cl2w_df) <- c("forecast-model", "percentage")
cl3w_df <- as.data.frame(as.table(colSums(cl3w)/nrow(cl3w)*100))
colnames(cl3w_df) <- c("forecast-model", "percentage")

Weekly_clust_df <- do.call("rbind", list(allW, cl1w_df, cl2w_df, cl3w_df))
Weekly_clust_df$source <- rep(c("all series", "Cluster 3", "Cluster 1", "Cluster 2"), each=9)
Weekly_clust_df$percentage <- round(Weekly_clust_df$percentage,2)
Weekly_clust_df


## Daily series
load("data/daily/monash_cluster_model_outputD/predict_daily_eval500.rda")
load("data/daily/dailyM4_forecast.rda")
models_dailyM4 <- combined_models(predict_daily_eval500, ncomp=4, 
                                  dailyM4_forecast,
                                  nseries=4227,
                                  h=14)
table(unlist(models_dailyM4))/4227*100
allD <- data.frame(table(unlist(models_dailyM4))/4227*100)
colnames(allD) <- c("forecast-model", "percentage")

# mstlarima   mstlets        nn        rw 
# 65.814999 44.334043 85.900166  9.462976 
# rwd    snaive     stlar     tbats 
# 8.729595  5.630471 72.888573 87.721789 
# theta        wn 
# 15.424651  4.092737 


mstlarima <- rep(0,4227)
mstlets <- rep(0,4227)
nn <- rep(0,4227)
rw <- rep(0,4227)
rwd <- rep(0,4227)
snaive <- rep(0,4227)
stlar <- rep(0,4227)
tbats <- rep(0,4227)
theta <- rep(0,4227)
wn <- rep(0,4227)
for(i in 1:4227){
  
  if (c("mstlarima") %in% models_dailyM4[[1]][[i]]){mstlarima[i]=1}
  if (c("mstlets") %in% models_dailyM4[[1]][[i]]){mstlets[i]=1}
  if (c("nn") %in% models_dailyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_dailyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_dailyM4[[1]][[i]]){rwd[i]=1}
  if (c("snaive") %in% models_dailyM4[[1]][[i]]){snaive[i]=1}
  if (c("stlar") %in% models_dailyM4[[1]][[i]]){stlar[i]=1}
  if (c("tbats") %in% models_dailyM4[[1]][[i]]){tbats[i]=1}
  if (c("theta") %in% models_dailyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_dailyM4[[1]][[i]]){wn[i]=1}
}

daily_cluster <- data.frame(mstlarima=mstlarima, mstlets=mstlets,nn=nn, rw=rw, rwd=rwd, snaive=snaive, stlar=stlar, tbats=tbats, 
                            theta=theta, wn=wn)
dist_daily <- dist(daily_cluster, method = "binary")
hc_daily <- hclust(dist_daily, method="ward")
daily_clust <- cutree(hc_daily, k = 3)
table(daily_clust)
save(daily_clust, file="data/daily/daily_clust.rda")
load("data/daily/daily_clust.rda")
daily_cluster$daily_clust <- daily_clust

cl1d <- dplyr::filter(daily_cluster, daily_clust==1) %>% select(c(-daily_clust))
cl2d <- dplyr::filter(daily_cluster, daily_clust==2) %>% select(c(-daily_clust))
cl3d <- dplyr::filter(daily_cluster, daily_clust==3) %>% select(c(-daily_clust))

cl1d_df <- as.data.frame(as.table(colSums(cl1d)/nrow(cl1d)*100))
colnames(cl1d_df) <- c("forecast-model", "percentage")
cl2d_df <- as.data.frame(as.table(colSums(cl2d)/nrow(cl2d)*100))
colnames(cl2d_df) <- c("forecast-model", "percentage")
cl3d_df <- as.data.frame(as.table(colSums(cl3d)/nrow(cl3d)*100))
colnames(cl3d_df) <- c("forecast-model", "percentage")

Daily_clust_df <- do.call("rbind", list(allD, cl1d_df, cl2d_df, cl3d_df))
Daily_clust_df$source <- rep(c("all series", "Cluster 3", "Cluster 1", "Cluster 2"), each=10)
Daily_clust_df$percentage <- round(Daily_clust_df$percentage,2)
Daily_clust_df

## Hourly series
load("data/hourly/monash_cluster_model_outputH/predict_hourly_eval500.rda")
load("data/hourly/hourlyM4_forecast.rda")
models_hourlyM4 <- combined_models(predict_hourly_eval500, ncomp=4, 
                                   hourlyM4_forecast,
                                   nseries=414,
                                   h=48)
table(unlist(models_hourlyM4))/414*100
allH <- data.frame(table(unlist(models_hourlyM4))/414*100)
colnames(allH) <- c("forecast-model", "percentage")

# mstlarima   mstlets        nn        rw 
# 78.985507 43.961353 68.840580 16.183575 
# rwd    snaive     stlar     tbats 
# 4.830918 14.251208 57.729469 91.304348 
# theta        wn 
# 2.898551 21.014493 



mstlarima <- rep(0,414)
mstlets <- rep(0,414)
nn <- rep(0,414)
rw <- rep(0,414)
rwd <- rep(0,414)
snaive <- rep(0,414)
stlar <- rep(0,414)
tbats <- rep(0,414)
theta <- rep(0,414)
wn <- rep(0,414)
for(i in 1:414){
  
  if (c("mstlarima") %in% models_dailyM4[[1]][[i]]){mstlarima[i]=1}
  if (c("mstlets") %in% models_dailyM4[[1]][[i]]){mstlets[i]=1}
  if (c("nn") %in% models_dailyM4[[1]][[i]]){nn[i]=1}
  if (c("rw") %in% models_dailyM4[[1]][[i]]){rw[i]=1}
  if (c("rwd") %in% models_dailyM4[[1]][[i]]){rwd[i]=1}
  if (c("snaive") %in% models_dailyM4[[1]][[i]]){snaive[i]=1}
  if (c("stlar") %in% models_dailyM4[[1]][[i]]){stlar[i]=1}
  if (c("tbats") %in% models_dailyM4[[1]][[i]]){tbats[i]=1}
  if (c("theta") %in% models_dailyM4[[1]][[i]]){theta[i]=1}
  if (c("wn") %in% models_dailyM4[[1]][[i]]){wn[i]=1}
}

hourly_cluster <- data.frame(mstlarima=mstlarima, mstlets=mstlets,nn=nn, rw=rw, rwd=rwd, snaive=snaive, stlar=stlar, tbats=tbats, 
                             theta=theta, wn=wn)
dist_hourly <- dist(hourly_cluster, method = "binary")
hc_hourly <- hclust(dist_hourly, method="ward")
hourly_clust <- cutree(hc_hourly, k = 3)
save(hourly_clust, file="data/hourly/hourly_clust.rda")
table(hourly_clust)
load("data/hourly/hourly_clust.rda")
hourly_cluster$hourly_clust <- hourly_clust

cl1h <- dplyr::filter(hourly_cluster, hourly_clust==1) %>% select(c(-hourly_clust))
cl2h <- dplyr::filter(hourly_cluster, hourly_clust==2) %>% select(c(-hourly_clust))
cl3h <- dplyr::filter(hourly_cluster, hourly_clust==3) %>% select(c(-hourly_clust))

cl1h_df <- as.data.frame(as.table(colSums(cl1h)/nrow(cl1h)*100))
colnames(cl1h_df) <- c("forecast-model", "percentage")
cl2h_df <- as.data.frame(as.table(colSums(cl2h)/nrow(cl2h)*100))
colnames(cl2h_df) <- c("forecast-model", "percentage")
cl3h_df <- as.data.frame(as.table(colSums(cl3h)/nrow(cl3h)*100))
colnames(cl3h_df) <- c("forecast-model", "percentage")

Hourly_clust_df <- do.call("rbind", list(allH, cl1h_df, cl2h_df, cl3h_df))
Hourly_clust_df$source <- rep(c("all series", "Cluster 1", "Cluster 2", "Cluster 3"), each=10)
Hourly_clust_df$percentage <- round(Hourly_clust_df$percentage,2)
Hourly_clust_df

## Combine the all dataframes of yearly, quarterly, monthly, weekly, daily and hourly
percentage_cal <- do.call("rbind", list(Yearly_clust_df,
                                        Quarterly_clust_df,
                                        Monthly_clust_df,
                                        Weekly_clust_df,
                                        Daily_clust_df,
                                        Hourly_clust_df))
percentage_cal$frequency <- c(rep("Yearly", 28), rep("Quarterly", 40), rep("Monthly", 40),
                              rep("Weekly", 36), rep("Daily", 40), rep("Hourly", 40))
save(percentage_cal, file="data/percentage_cal.rda")

## ---- radarplot
load("data/percentage_cal.rda")
# create new coord : inherit coord_polar
coord_radar <- 
  function(theta='x', start=0, direction=1){
    # input parameter sanity check
    match.arg(theta, c('x','y'))
    
    ggproto(
      NULL, CoordPolar, 
      theta=theta, r=ifelse(theta=='x','y','x'),
      start=start, direction=sign(direction),
      is_linear=function() TRUE)
  }

percentage_cal$percentage <- round(percentage_cal$percentage,1)
colnames(percentage_cal) <- c("forecast", "percentage", "source", "frequency")
percentage_cal$source <- factor(percentage_cal$source, levels = c("all series","Cluster 1", "Cluster 2", "Cluster 3"))
percentage_cal$frequency <- factor(percentage_cal$frequency, levels = c("Yearly","Quarterly", "Monthly", "Weekly",
                                                                        "Daily", "Hourly"))
percentage_cal$forecast <- factor(percentage_cal$forecast, levels = c("arima","ets", "theta", "rwd",
                                                                        "rw", "nn", "stlar", "snaive", "tbats",
                                                                      "wn", "mstlarima", "mstlets"))

ggplot(percentage_cal,
       aes(x = forecast,
           y = percentage,
           fill=source,
           col=source)) +
 # geom_line(aes(color = factor(source)),  size = 1) +
  geom_bar( colour="black",width=1,stat="identity")+
  facet_grid(source~frequency)+
  theme(strip.text.x = element_text(size = rel(2)),
        strip.text.y = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +coord_polar()+scale_color_manual(values=c("#000000","#1b9e77","#d95f02", "#7570b3"))+
        scale_fill_manual(values=c("#000000","#1b9e77","#d95f02", "#7570b3"))+theme(legend.position = "none")+ylab("")+xlab("")


## ---- clusterplot
## Packages-----------------------------------------------------------------
library(Rtsne)
library(tidyverse)
## Yearly time series
# Load yearly training dataset
load("data/yearly/yearly_features_training.rda")
load("data/yearly/features_M4Y.rda")
load("data/yearly/yearly_clust.rda")
## bind training data and features of M4 competition data into one column
yearly_tsne <- dplyr::bind_rows(yearly_features_training, features_M4Y)
yearly_tsne$source <- c(rep("M1", 181),rep("M3",645),rep("Simulated", 10000), rep("M4", 23000))
yearly_tsne$cluster <- c(rep(NA, 10826), yearly_clust)
yearly_tsne_unique <- unique(yearly_tsne)
yearly_matrix <- as.matrix(yearly_tsne_unique[,1:25])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_yearly <- Rtsne(yearly_matrix) # Run TSNE
tsne_plot_yearly <- data.frame(x = tsne_out_yearly$Y[,1], y = tsne_out_yearly$Y[,2], col = yearly_tsne_unique$source,
                               cluster = yearly_tsne_unique$cluster,
                               trend=yearly_tsne_unique$trend,
                               linearity=yearly_tsne_unique$linearity,
                               e_acf1=yearly_tsne_unique$e_acf1,
                               seasonality = rep(0, 33788))
tsne_plot_yearly$col <- factor(tsne_plot_yearly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsne_plot_yearly$cluster <- factor(tsne_plot_yearly$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_yearly$cluster) <- list( "Cluster 1"="2", "Cluster 2"="1", "Cluster 3"="3")

tsneY <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_yearly, col=="M4"),aes(x=x, y=y, color=cluster), alpha=0.5)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Yearly")




## Packages
library(Rtsne)
library(tidyverse)
## quarterly time series
# Load quarterly training dataset
load("data/quarterly/quarterly_features_training.rda")
load("data/quarterly/features_M4Q.rda")
load("data/quarterly/quarterly_clust.rda")
## bind training data and features of M4 competition data into one column
quarterly_tsne <- dplyr::bind_rows(quarterly_features_training, features_M4Q)
quarterly_tsne$source <- c(rep("M1", 203),rep("M3",756),rep("Simulated", 10000), rep("M4", 24000))
quarterly_tsne$cluster <- c(rep(NA, 10959), quarterly_clust)
quarterly_tsne_unique <- unique(quarterly_tsne)
quarterly_matrix <- as.matrix(quarterly_tsne_unique[,1:30])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_quarterly <- Rtsne(quarterly_matrix) # Run TSNE
tsne_plot_quarterly <- data.frame(x = tsne_out_quarterly$Y[,1], y = tsne_out_quarterly$Y[,2], col = quarterly_tsne_unique$source,
                               cluster = quarterly_tsne_unique$cluster,
                               trend=quarterly_tsne_unique$trend,
                               linearity=quarterly_tsne_unique$linearity,
                                 e_acf1=quarterly_tsne_unique$e_acf1,
                               seasonality = quarterly_tsne_unique$seasonality)
tsne_plot_quarterly$col <- factor(tsne_plot_quarterly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsne_plot_quarterly$cluster <- factor(tsne_plot_quarterly$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_quarterly$cluster) <- list( "Cluster 1"="1", "Cluster 2"="2", "Cluster 3"="3")
tsneQ <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_quarterly, col=="M4"),aes(x=x, y=y, color=cluster))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Quarterly")
# 


## Packages
library(Rtsne)
library(tidyverse)
## monthly time series
# Load monthly training dataset
load("data/monthly/monthly_features_training.rda")
load("data/monthly/features_M4M.rda")
load("data/monthly/monthly_clust.rda")
load("data/monthly/sample_id.rda")
## bind training data and features of M4 competition data into one column
features_M4M_sample <- features_M4M[sample_id,]
monthly_tsne <- dplyr::bind_rows(monthly_features_training, features_M4M_sample)
monthly_tsne$source <- c(rep("M1", 617),rep("M3",1428),rep("Simulated", 10000), rep("M4", 30000))
monthly_tsne$cluster <- c(rep(NA, 12045), monthly_clust)
monthly_matrix <- as.matrix(monthly_tsne[,1:30])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_monthly <- Rtsne(monthly_matrix, check_duplicates = FALSE) # Run TSNE
tsne_plot_monthly <- data.frame(x = tsne_out_monthly$Y[,1], y = tsne_out_monthly$Y[,2], col = monthly_tsne$source,
                                  cluster =monthly_tsne$cluster,
                                trend=monthly_tsne$trend,
                                linearity=monthly_tsne$linearity,
                                e_acf1=monthly_tsne$e_acf1,
                                seasonality = monthly_tsne$seasonality)
tsne_plot_monthly$col <- factor(tsne_plot_monthly$col, levels = c("Simulated", "M4", "M1", "M3"))
tsne_plot_monthly$cluster <- factor(tsne_plot_monthly$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_monthly$cluster) <- list( "Cluster 1"="2", "Cluster 2"="1", "Cluster 3"="3")

tsneM <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_monthly, col=="M4"),aes(x=x, y=y, color=cluster))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Monthly")


## Packages
library(Rtsne)
library(tidyverse)
## weekly time series
# Load weekly training dataset
load("data/weekly/features_weekly_marbased_x.rda")
load("data/weekly/features_M4W.rda")
load("data/weekly/weekly_clust.rda")
## bind training data and features of M4 competition data into one column
weekly_tsne <- dplyr::bind_rows(features_weekly_marbased_x, features_M4W)
weekly_tsne$source <- c(rep("Simulated", 10000), rep("M4", 359))
weekly_tsne$cluster <- c(rep(NA, 10000), weekly_clust)
weekly_tsne_unique <- unique(weekly_tsne)
weekly_matrix <- as.matrix(weekly_tsne_unique[,1:27])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_weekly <- Rtsne(weekly_matrix) # Run TSNE
tsne_plot_weekly <- data.frame(x = tsne_out_weekly$Y[,1], y = tsne_out_weekly$Y[,2], col = weekly_tsne_unique$source,
                                cluster = weekly_tsne_unique$cluster,
                               trend=weekly_tsne_unique$trend,
                               linearity=weekly_tsne_unique$linearity,
                               e_acf1=weekly_tsne_unique$e_acf1,
                               seasonality = weekly_tsne_unique$seasonality)
tsne_plot_weekly$col <- factor(tsne_plot_weekly$col, levels = c("Simulated", "M4"))
tsne_plot_weekly$cluster <- factor(tsne_plot_weekly$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_weekly$cluster) <- list( "Cluster 1"="2", "Cluster 2"="3", "Cluster 3"="1")
tsneW <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=cluster))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Weekly")

## Packages
library(Rtsne)
library(tidyverse)
## daily time series
# Load dailytraining dataset
load("data/daily/features_daily_marbased.rda")
load("data/daily/features_M4D.rda")
load("data/daily/daily_clust.rda")
## bind training data and features of M4 competition data into one column
daily_tsne <- dplyr::bind_rows(features_daily_marbased, features_M4D)
daily_tsne$source <- c(rep("Simulated", 10000), rep("M4", 4227))
daily_tsne$cluster <- c(rep(NA, 10000), daily_clust)
daily_tsne_unique <- unique(daily_tsne)
daily_matrix <- as.matrix(daily_tsne_unique[,1:26])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_daily <- Rtsne(daily_matrix) # Run TSNE
tsne_plot_daily <- data.frame(x = tsne_out_daily$Y[,1], y = tsne_out_daily$Y[,2], col = daily_tsne_unique$source,
                               cluster = daily_tsne_unique$cluster,
                              trend=daily_tsne_unique$trend,
                              linearity=daily_tsne_unique$linearity,
                              e_acf1=daily_tsne_unique$e_acf1,
                              seasonality = daily_tsne_unique$seasonal_strength1)
tsne_plot_daily$col <- factor(tsne_plot_daily$col, levels = c("Simulated", "M4"))
tsne_plot_daily$cluster <- factor(tsne_plot_daily$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_daily$cluster) <- list( "Cluster 1"="2", "Cluster 2"="3", "Cluster 3"="1")

tsneD <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=cluster))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Daily")

# tsneDtrend <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
#   geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
#   xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ 
#   theme(legend.title = element_blank()) + ggtitle("Daily")+scale_color_gradientn(colours = terrain.colors(7))
# 
# tsneDlinearity <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
#   geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
#   xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ 
#   theme(legend.title = element_blank()) + ggtitle("Daily")+scale_color_gradientn(colours = terrain.colors(7))
# 
# tsneDseasonality <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
#   geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
#   xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ 
#   theme(legend.title = element_blank()) + ggtitle("Daily")+scale_color_gradientn(colours = terrain.colors(7))
# 
# tsneDe_acf1 <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
#   geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=e_acf1))+scale_alpha(guide = 'none')+
#   xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ 
#   theme(legend.title = element_blank()) + ggtitle("Daily")+scale_color_gradientn(colours = terrain.colors(7))



## Packages
library(Rtsne)
library(tidyverse)
## hourly time series
# Load hourly training dataset
load("data/hourly/features_hourly_marbased.rda")
load("data/hourly/features_M4H.rda")
load("data/hourly/hourly_clust.rda")
## bind training data and features of M4 competition data into one column
hourly_tsne <- dplyr::bind_rows(features_hourly_marbased, features_M4H)
hourly_tsne$source <- c(rep("Simulated", 10000), rep("M4", 414))
hourly_tsne$cluster <- c(rep(NA, 10000), hourly_clust)
hourly_tsne_unique <- unique(hourly_tsne)
hourly_matrix <- as.matrix(hourly_tsne_unique[,1:26])
set.seed(42) # Set a seed if you want reproducible results
tsne_out_hourly <- Rtsne(hourly_matrix) # Run TSNE
tsne_plot_hourly <- data.frame(x = tsne_out_hourly$Y[,1], y = tsne_out_hourly$Y[,2], col = hourly_tsne_unique$source,
                              cluster = hourly_tsne_unique$cluster,
                              trend=hourly_tsne_unique$trend,
                              linearity=hourly_tsne_unique$linearity,
                              e_acf1=hourly_tsne_unique$e_acf1,
                              seasonality = hourly_tsne_unique$seasonal_strength2)
tsne_plot_hourly$col <- factor(tsne_plot_hourly$col, levels = c("Simulated", "M4"))
tsne_plot_hourly$cluster <- factor(tsne_plot_hourly$cluster, levels = c("1", "2", "3"))
levels(tsne_plot_hourly$cluster) <- list( "Cluster 1"="1", "Cluster 2"="2", "Cluster 3"="3")

tsneH <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=cluster))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  theme(legend.title = element_blank()) + ggtitle("Hourly")

(tsneY|tsneQ|tsneM)/(tsneW|tsneD|tsneH)

## ---- featureplotsyearly

#yearly
b <- c(0., .5, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
tsneTrend <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_yearly, col=="M4"),aes(x=x, y=y, color=trend), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Yearly: trend")+ scale_color_gradientn(limits = c(0,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )
b <- c(0.5, 0.8, 1)
tsnerandomness <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_yearly, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.4)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Yearly: e_acf1")+ scale_color_gradientn(limits = c(0.5,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-10,5, 10)
tsnelinearity <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y), colour = NA)+
   geom_point(data=subset(tsne_plot_yearly, col=="M4"),aes(x=x, y=y, color=linearity), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Yearly: linearity")+ 
  scale_color_gradientn(limits = c(-10,10), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-1,0, 1)
tsnernd <- ggplot(tsne_plot_yearly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_yearly, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Yearly: e_acf1")+ 
  scale_color_gradientn(limits = c(-1,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

(tsneTrend|tsnelinearity|tsnernd )+theme(plot.margin=margin(l=-0.2,unit="cm"))

## ---- featureplotsquarterly
b <- c(0, 0.4, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
#Quarterly
tsneQtrend <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y), colour = NA)+
   geom_point(data=subset(tsne_plot_quarterly, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
   xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
   theme(legend.title = element_blank()) + ggtitle("Quarterly: trend")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-20, 0, 20) 
tsneQlinearity <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y), colour = NA)+
   geom_point(data=subset(tsne_plot_quarterly, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
   theme(legend.title = element_blank()) + ggtitle("Quarterly: linearity")+
  scale_color_gradientn(limits = c(-20,20),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )



b <- c( 0,0.6, 1) 
tsneQseasonality <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_quarterly, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Quarterly: seasonality")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-1,0, 1)
tsnerndQ <- ggplot(tsne_plot_quarterly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_quarterly, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Quarterly: e_acf1")+ 
  scale_color_gradientn(limits = c(-1,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

tsneQtrend|tsneQlinearity|tsneQseasonality|tsnerndQ

## ---- featureplotsmonthly
b <- c(0, 0.4, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
# Monthly
tsneMtrend <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_monthly, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
 theme(legend.title = element_blank()) + ggtitle("Monthly: trend")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-20, 0, 30)
tsneMlinearity <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_monthly, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
 theme(legend.title = element_blank()) + ggtitle("Monthly: linearity")+
  scale_color_gradientn(limits = c(-20,30),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )
# 

# 
b <- c(-0.9, 0, 0.9)
tsneMseasonality <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_monthly, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Monthly: seasonality")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-1,0, 1)
tsnerndM <- ggplot(tsne_plot_monthly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_monthly, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Monthly: e_acf1")+ 
  scale_color_gradientn(limits = c(-1,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

tsneMtrend|tsneMlinearity|tsneMseasonality|tsnerndM 
## ---- featureplotsweekly
# Weekly
b <- c(0, 0.4, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
tsneWtrend <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Weekly: trend")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(0, 0.4, 1)
tsneWseasonality <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Weekly: seasonality")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-40, 20, 40)
tsneWlinearity <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+theme(aspect.ratio=1)+
theme(legend.title = element_blank()) + ggtitle("Weekly: linearity")+
  scale_color_gradientn(limits = c(-40,40),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-1,0, 1)
tsnerndW <- ggplot(tsne_plot_weekly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_weekly, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Weekly: e_acf1")+ 
  scale_color_gradientn(limits = c(-1,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

tsneWtrend|tsneWlinearity|tsneWseasonality|tsnerndW

## ---- featureplotsdaily
# daily
b <- c(0, 0.5, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
tsneDtrend <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Daily: trend")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-80, 20, 80)
tsneDlinearity <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Daily: linearity")+
  scale_color_gradientn(limits = c(-80,80),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )
# 
b <- c(0, 0.1, 0.4)
tsneDseasonality <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
 theme(legend.title = element_blank()) + ggtitle("Daily: seasonality")+
  scale_color_gradientn(limits = c(0,0.4),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-1,0.2, 1)
tsnerndD <- ggplot(tsne_plot_daily) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_daily, col=="M4"),aes(x=x, y=y, color=e_acf1), alpha=0.3)+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Daily: e_acf1")+ 
  scale_color_gradientn(limits = c(-1,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )
tsneDtrend|tsneDlinearity|tsneDseasonality|tsnerndD

## ---- featureplotshourly
b <- c(0, 0.25, 1)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
# hourly
tsneHtrend <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=trend))+scale_alpha(guide = 'none')+
 xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Hourly: trend")+
  scale_color_gradientn(limits = c(0,1),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )
# 
b <- c(-15, 0, 20)
colors <- c('navyblue', 'darkmagenta', 'darkorange1')
tsneHlinearity <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y), colour = NA)+
geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=linearity))+scale_alpha(guide = 'none')+
xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
theme(legend.title = element_blank()) + ggtitle("Hourly: linearity")+
  scale_color_gradientn(limits = c(-15,20),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

# 
b <- c(0, 0.2, 0.9)
tsneHseasonality <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y), colour = NA)+
 geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=seasonality))+scale_alpha(guide = 'none')+
xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
theme(legend.title = element_blank()) + ggtitle("Hourly:seasonality")+
  scale_color_gradientn(limits = c(0,0.9),colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

b <- c(-0.5,0.5, 1)
tsnerndH <- ggplot(tsne_plot_hourly) + geom_point(aes(x=x, y=y), colour = NA)+
  geom_point(data=subset(tsne_plot_hourly, col=="M4"),aes(x=x, y=y, color=e_acf1))+scale_alpha(guide = 'none')+
  xlab("t-SNE Component 1")+ylab("t-SNE Component 2")+ theme(aspect.ratio=1)+
  theme(legend.title = element_blank()) + ggtitle("Hourly: e_acf1")+ 
  scale_color_gradientn(limits = c(-0.5,1), colors = colors, breaks = b, labels = format(b))+
  theme(
    plot.title = element_text(size=24),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    legend.text=element_text(size=18)
  )

tsneHtrend|tsneHlinearity|tsneHseasonality|tsnerndH
