# Point forecast
##
## ----packages
library(movingknots)
library(flutils)
library(fformpp)
library(seer)
library(M4comp2018)

# Yearly++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ----models on yearly data
yearly_M4 <- Filter(function(l) l$period == "Yearly", M4)
## yearly M4competition MASE value matrix
load("data/yearly/yearlyM4MASE_accuracy.rda")#real.MASE
## features of the yearly M4-competition data
load("data/yearly/features_M4Y.rda")
model.names <- c("ets","arima", "rw", "rwd", "wn", "theta","nn")
## Forecast list
load("data/yearly/yearlyM4_forecasts.rda")
#------------------------------------------------------------------
## yearly_niter500.rda
load("data/yearly/monash_cluster_model_output/yearly_niter500.rda")
## yearly M4competition MASE value matrix
load("data/yearly/yearlyM4MASE_accuracy.rda")#real.MASE
## features of the yearly M4-competition data
load("data/yearly/features_M4Y.rda")
model.names <- c("ets","arima", "rw", "rwd", "wn", "theta","nn")
load("data/yearly/yearlyM4_forecasts.rda")
predict_yearly_eval500 <- predict_fformpp(yearly_niter500, features_M4Y, 
                                          model.names,
                                          log = TRUE,
                                          final.estimate=median)
save(predict_yearly_eval500, file="data/yearly/monash_cluster_model_output/predict_yearly_eval500.rda")

load("data/yearly/monash_cluster_model_output/predict_yearly_eval500.rda")
yearly_eval500 <- individual_forecast(predicted=predict_yearly_eval500, 
                                      accmat=cal_MASE, 
                                      real.error=yearlyM4MASE_accuracy, 
                                      tslist=yearly_M4, h=6,
                                      forecast_list=yearlyM4_forecasts)
yearly_eval500$summary
#           our_method ets    arima       rw
# mean     3.362080 3.444418 3.401363 3.974360
# median   2.292569 2.328707 2.314717 2.936964
#           rwd       wn    theta       nn
# mean   3.068413 13.42076 3.374633 4.055065
# median 2.090543 10.70306 2.312369 2.633026
save(yearly_eval500, file="data/yearly/monash_cluster_model_output/yearly_eval500.rda")

###------------combination forecast
yearly_eval500_nc4_med <- combination_forecast(predicted=predict_yearly_eval500, 
                                               accmat=cal_MASE, 
                                               ncomp=4,
                                               real.error=yearlyM4MASE_accuracy, 
                                               tslist=yearly_M4, h=6,
                                               forecast_list=yearlyM4_forecasts,
                                               weights=FALSE, measure="median")
yearly_eval500_nc4_med$summary
#         our_method_comb      ets    arima
# mean          3.079618 3.444418 3.401363
# median        2.111864 2.328707 2.314717
#           rw      rwd       wn    theta
# mean   3.974360 3.068413 13.42076 3.374633
# median 2.936964 2.090543 10.70306 2.312369
#           nn
# mean   4.055065
# median 2.633026



# Quarterly+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
quarterly_M4 <- Filter(function(l) l$period == "Quarterly", M4)
## quaterly_niter500.rda
load("data/quarterly/monash_cluster_model_outputQ/quarterly_niter500.rda")
## yearly M4competition MASE value matrix
load("data/quarterly/quarterlyM4MASE_accuracy.rda")#real.MASE
## features of the yearly M4-competition data
load("data/quarterly/features_M4Q.rda")
model.names <- c("ets","arima", "rw", "rwd", "wn", "theta","stlar","nn", "snaive", "tbats")
## Forecast list
load("data/quarterly/quarterlyM4_forecast.rda")

predict_quarterly_eval500 <- predict_fformpp(quarterly_niter500, features_M4Q, 
                                             model.names,
                                             log = TRUE)

save(predict_quarterly_eval500, file="data/quarterly/monash_cluster_model_outputQ/predict_quarterly_eval500.rda")

quarterly_eval500 <- individual_forecast(predicted=predict_quarterly_eval500, 
                                         accmat=cal_MASE, 
                                         real.error=quarterlyM4MASE_accuracy, 
                                         tslist=quarterly_M4, h=8,
                                         forecast_list=quarterlyM4_forecast)
quarterly_eval500$summary
quarterly_eval500$summary
#        our_method       ets     arima       rw
# mean    1.1827131 1.1607206 1.1650625 1.477034
# median  0.9134703 0.8861277 0.8950737 1.163915
#          rwd       wn     theta    stlar       nn
# mean   1.329629 6.501963 1.2424681 2.028835 1.550323
# median 1.005724 4.660368 0.9770203 1.679611 1.120199
#          snaive     tbats
# mean   1.602247 1.1856899
# median 1.309740 0.9146604
save(quarterly_eval500, file="data/quarterly/monash_cluster_model_outputQ/quarterly_eval500.rda")

## combination ---------------------
load("data/quarterly/monash_cluster_model_outputQ/predict_quarterly_eval500.rda")
combination_eval500 <- combination_forecast(predicted=predict_quarterly_eval500, 
                                            accmat=cal_MASE, 
                                            ncomp=4,
                                            real.error=quarterlyM4MASE_accuracy, 
                                            tslist=quarterly_M4, h=8,
                                            forecast_list=quarterlyM4_forecast,
                                            weights=FALSE, measure="median")
combination_eval500$summary
#         our_method_comb       ets     arima
# mean         1.1311970 1.1607206 1.1650625
# median       0.8724489 0.8861277 0.8950737
#           rw      rwd       wn     theta
# mean   1.477034 1.329629 6.501963 1.2424681
# median 1.163915 1.005724 4.660368 0.9770203
#         stlar       nn   snaive     tbats
# mean   2.028835 1.550323 1.602247 1.1856899
# median 1.679611 1.120199 1.309740 0.9146604

# Monthly+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
## monthly_niter500.rda
load("data/monthly/monash_cluster_model_outputM/monthly_niter500.rda")
## monthly M4competition MASE value matrix
load("data/monthly/monthlyM4MASE_accuracy.rda")#real.MASE
## features of the monthly M4-competition data
load("data/monthly/features_M4M.rda")
model.names <- c("ets","arima", "rw", "rwd", "wn", "theta","stlar","nn", "snaive", "tbats")
## Forecast list
load("data/monthly/monthlyM4_forecast.rda")
predict_monthly_eval500 <- predict_fformpp(monthly_niter500, features_M4M, 
                                           model.names,
                                           log = TRUE,
                                           final.estimate = median)
save(predict_monthly_eval500, file="data/monthly/monash_cluster_model_outputM/predict_monthly_eval500.rda")

load("data/monthly/monash_cluster_model_outputM/predict_monthly_eval500.rda")
monthly_eval500 <- individual_forecast(predicted=predict_monthly_eval500, 
                                       accmat=cal_MASE, 
                                       real.error=monthlyM4MASE_accuracy, 
                                       tslist=monthly_M4, h=18,
                                       forecast_list=monthlyM4_forecasts)
monthly_eval500$summary
#       our_method       ets     arima
# mean    1.0537601 0.9478268 0.9301770
# median  0.7340908 0.7355383 0.7254596
# rw       rwd       wn     theta
# mean   1.2051290 1.1801760 4.108978 0.9650073
# median 0.9021903 0.8646106 2.508766 0.7526664
#         stlar        nn   snaive     tbats
# mean   1.333105 1.1395722 1.259717 1.0524175
# median 1.049182 0.8740616 1.002555 0.7327798


monthly_eval500_mean_combmed <- combination_forecast(predicted=predict_monthly_eval500_mean, 
                                                     accmat=cal_MASE, 
                                                     ncomp=4,
                                                     real.error=monthlyM4MASE_accuracy, 
                                                     tslist=monthly_M4, h=18,
                                                     forecast_list=monthlyM4_forecast,
                                                     weights=FALSE, measure="median")
monthly_eval500_mean_combmed$summary
#         our_method_comb       ets     arima
# mean         0.8988798 0.9478268 0.9301770
# median       0.7028440 0.7355383 0.7254596
#           rw       rwd       wn     theta
# mean   1.2051290 1.1801760 4.108978 0.9650073
# median 0.9021903 0.8646106 2.508766 0.7526664
#         stlar        nn   snaive     tbats
# mean   1.333105 1.1395722 1.259717 1.0524175
# median 1.049182 0.8740616 1.002555 0.7327798


# Weekly ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
weekly_M4 <- Filter(function(l) l$period == "Weekly", M4)
## weekly_niter500.rda
load("data/weekly/monash_cluster_model_outputW/weekly_niter500.rda")
## weekly M4competition MASE value matrix
load("data/weekly/weeklyM4MASE_accuracy.rda")#real.MASE
## features of the weekly M4-competition data
load("data/weekly/features_M4W.rda")
model.names <- c("arima", "rw", "rwd", "wn", "theta", "stlar", "nn", "snaive", "tbats")
## Forecast list
load("data/weekly/weeklyM4_forecast.rda")
predict_weekly_eval500 <- predict_fformpp(weekly_niter500, features_M4W, 
                                          model.names,
                                          log = TRUE,
                                          final.estimate = mean)
save(predict_weekly_eval500, file="data/weekly/monash_cluster_model_outputW/predict_weekly_eval500.rda")

load("data/weekly/monash_cluster_model_outputW/predict_weekly_eval500.rda")
weekly_eval500 <- individual_forecast(predicted=predict_weekly_eval500, 
                                      accmat=cal_MASE, 
                                      real.error=weeklyM4MASE_accuracy, 
                                      tslist=weekly_M4, h=13,
                                      forecast_list=weeklyM4_forecasts)
weekly_eval500$summary
#           our_method    arima       rw      rwd
# mean     2.532727 2.554711 2.777295 2.682475
# median   1.589496 1.647024 1.938420 1.864447
#             wn    theta    stlar       nn
# mean   49.90950 2.638685 3.145209 4.024351
# median 14.06692 1.928239 1.842560 2.006711
#         snaive    tbats
# mean   2.777295 2.492555
# median 1.938420 1.682433

#=================================================================
# Combination forecast
weekly_eval500_mean_combmed4 <- combination_forecast(predicted=predict_weekly_eval500, 
                                                     accmat=cal_MASE, 
                                                     ncomp=4,
                                                     real.error=weeklyM4MASE_accuracy, 
                                                     tslist=weekly_M4, h=13,
                                                     forecast_list=weeklyM4_forecast,
                                                     weights=FALSE, measure="median")
weekly_eval500_mean_combmed4$summary
#         our_method_comb    arima       rw
# mean          2.460802 2.554711 2.777295
# median        1.762029 1.647024 1.938420
#           rwd       wn    theta    stlar
# mean   2.682475 49.90950 2.638685 3.145209
# median 1.864447 14.06692 1.928239 1.842560
#         nn   snaive    tbats
# mean   4.024351 2.777295 2.492555
# median 2.006711 1.938420 1.682433


# Daily ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
daily_M4 <- Filter(function(l) l$period == "Daily", M4)
## daily_niter500.rda
dmodel <- load("data/daily/monash_cluster_model_outputD/daily_niter500.rda") # because I save the model as daily_niter100(by mistake) it should be daily_niter500
daily_model500 <- get(dmodel)
## daily M4competition MASE value matrix
load("data/daily/benchmark_accuracy_daily/accuracy_Daily_1mase.rda")#real.MASE
## features of the weekly M4-competition data
load("data/daily/features_M4D.rda")
features_M4D <- within(features_M4D, rm(spikiness))
model.names <- c("rw", "rwd", "wn", "theta", "stlar",
                 "nn", "snaive", "mstlets", "mstlarima", "tbats")
## Forecast list
load("data/daily/dailyM4_forecast.rda")
predict_daily_eval500 <- predict_fformpp(daily_model500, features_M4D, 
                                         model.names,
                                         log = TRUE,
                                         final.estimate = mean)
save(predict_daily_eval500, file="data/daily/monash_cluster_model_outputD/predict_daily_eval500.rda")

#load("data/hourly/monash_cluster_model_outputH/predict_hourly_eval100.rda")
daily_eval500 <- individual_forecast(predicted=predict_daily_eval500, 
                                     accmat=cal_MASE, 
                                     real.error=accuracy_Daily_1mase, 
                                     tslist=daily_M4, h=14,
                                     forecast_list=dailyM4_forecasts)
daily_eval500$summary
#        our_method       rw      rwd
# mean     4.263795 3.278424 3.251999
# median   2.675542 2.355650 2.324329
#              wn theta    stlar       nn
# mean   38.07761    NA 4.495883 3.904392
# median 22.01963    NA 2.971114 2.700808
#          snaive  mstlets mstlarima
# mean   24.46473 3.732609  3.844741
# median 12.82242 2.520663  2.622903
#        tbats
# mean   3.279914
# median 2.371716

mean(accuracy_Daily_1mase[,"theta"],na.rm = TRUE)
#3.330422
#=================================================================
# Combination forecast
daily_eval500_mean_combmed4 <- combination_forecast(predicted=predict_daily_eval500, 
                                                    accmat=cal_MASE, 
                                                    ncomp=4,
                                                    real.error=accuracy_Daily_1mase, 
                                                    tslist=daily_M4, h=14,
                                                    forecast_list=dailyM4_forecast,
                                                    weights=FALSE, measure="median")
daily_eval500_mean_combmed4$summary
#      our_method_comb       rw      rwd
# mean          3.620388 3.278424 3.251999
# median        2.478556 2.355650 2.324329
#         wn      theta    stlar       nn
# mean   38.07761    NA 4.495883 3.904392
# median 22.01963    NA 2.971114 2.700808
#       snaive  mstlets mstlarima
# mean   24.46473 3.732609  3.844741
# median 12.82242 2.520663  2.622903
#        tbats
# mean   3.279914
# median 2.371716

# Hourly ++++++++++++++++++++++++++++++++++++++++++++
# WITH 500 ITERATIONS MODEL
## hourly_niter500.rda
load("data/hourly/monash_cluster_model_outputH/hourly_niter500.rda")
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)
## features of the weekly M4-competition data
load("data/hourly/features_M4H.rda")
features_M4H <- within(features_M4H, rm(N))
model.names <- c("rw", "rwd", "wn", "theta", "stlar",
                 "nn", "snaive", "mstlets", "mstlarima", "tbats")
## Forecast list
load("data/hourly/hourlyM4_forecast.rda")
predict_hourly_eval500 <- predict_fformpp(hourly_niter500, features_M4H, 
                                          model.names,
                                          log = TRUE,
                                          final.estimate = mean)
save(predict_hourly_eval500, file="data/hourly/monash_cluster_model_outputH/predict_hourly_eval500.rda")
load("data/hourly/benchmark_accuracy_hourly/accuracy_Hourly_24mase.rda")#real.MASE

hourly_eval500_24mase <- individual_forecast(predicted=predict_hourly_eval500, 
                                             accmat=cal_MASE, 
                                             real.error=accuracy_Hourly_24mase, 
                                             tslist=hourly_M4, h=48,
                                             forecast_list=hourlyM4_forecasts)
hourly_eval500_24mase$summary
#         our_method        rw       rwd        wn
# mean    1.0659619 11.607687 11.455023 11.686212
# median  0.8215616  3.684926  3.710522  3.345248
#         theta    stlar       nn   snaive
# mean   1.595747 1.495153 1.098893 2.867223
# median 1.053041 1.142724 0.894241 1.397356
#           mstlets mstlarima     tbats
# mean   1.2397652  1.125557 1.3018687
# median 0.7481415  0.751914 0.9966678

hourly_eval500_mean_combmed4_24mase <- combination_forecast(predicted=predict_hourly_eval500, 
                                                            accmat=cal_MASE, 
                                                            ncomp=4,
                                                            real.error=accuracy_Hourly_24mase, 
                                                            tslist=hourly_M4, h=48,
                                                            forecast_list=hourlyM4_forecast,
                                                            weights=FALSE, measure="median")
hourly_eval500_mean_combmed4_24mase$summary
#        our_method_comb        rw       rwd
# mean         0.9678904 11.607687 11.455023
# median       0.7625883  3.684926  3.710522
#           wn    theta    stlar       nn
# mean   11.686212 1.595747 1.495153 1.098893
# median  3.345248 1.053041 1.142724 0.894241
#           snaive   mstlets mstlarima     tbats
# mean   2.867223 1.2397652  1.125557 1.3018687
# median 1.397356 0.7481415  0.751914 0.9966678