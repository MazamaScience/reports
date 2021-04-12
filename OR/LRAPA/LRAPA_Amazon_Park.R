################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #1 (MazamaScience/reports)
# Date: 4/12/2021
################################################################
# Now that we have found colocate sensors (within 100 m from the closest FRM 
# monitor) we can start exploring the pat files by running pat_scatterPlotMatrix()
# pat_internalFit and pat_externalFit, and select the best performing sensors.
# We'll then:
# 1) combine sensor hourly data (PM2.5, temperature, humidity) with monitor
# hourly data (PM2.5).
# 2) fit linear and multilinear models to explore the influence of humidity on the 
# sensor performance.
# 3) compare results from step 2) to linearly modeled predicted data.   


# ----- Setup --------------------------------------------------------------
# * libraries -----
library(MazamaCoreUtils)   
library(AirSensor)
library(PWFSLSmoke)
library(fBasics)
library(car)
library(MASS)

# * archiveDir -----
archiveDir <- "C:/Users/astri/Mirror/Mazamascience/Projects/Data/LRAPA"

# * load sensors and monitors data -----
# load LRAPA sensor and monitirs file from archiveDir 
LRAPA_sensors <- get(load(file.path(archiveDir, "LRAPA_pas.rda")))
LRAPA_monitors <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))

# ----- Create a table including only $meta useful to your analysis ------------
near_sensors_tb <- LRAPA_sensors %>%
  pas_filter(pwfsl_closestDistance <= 100) %>%
  dplyr::select(label, 
         deviceDeploymentID, 
         pwfsl_closestMonitorID, 
         pwfsl_closestDistance, 
         DEVICE_LOCATIONTYPE)

# ----- Load pat data ----------------------------------------------------------
# * Setup -----
setArchiveBaseDir(archiveDir)
getArchiveBaseDir()

startdate = 20200701
enddate = 20201101
timezone = "America/Los_Angeles"

# * Amazon_Park -----
Amazon_Park <- pat_load(
  id = "947c72aa269258cc_56971", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

# ----- Explore pat data -------------------------------------------------------
pat_multiplot(Amazon_Park)
pat_scatterPlotMatrix(Amazon_Park)
AM_lm <- pat_internalFit(Amazon_Park, showPlot = TRUE)
summary(AM_lm)
AM_lm_ex <- pat_externalFit(Amazon_Park, showPlot = TRUE)
summary(AM_lm_ex)

# ----- Linear model PM2.5 monitor ~ PM2.5 sensor for specific weeks -----------
# * Setup sensorMonitorFit() 
# 1) Open LRAPA_utils.R script 
# 2) "Source" the script 
# 3) Check that you have sensorMonitorFit() in your R environment 

# * July 01-08 data frame: sensor and monitor hourly data
# low PM25 week according to pat_internalFit() plot  
AP_df_07<- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200701,
  enddate = 20200708)
View(AP_df_07)
# note: all columns come from the sensor's pat object exept for "monitor_pm25"

# * run lm ----
AP_lm <- lm(AP_df_07$pm25_monitor ~ AP_df_07$pm25)

# check lm results
summary(AP_lm)

# create a lm results df
lm07_results <- data.frame(
  round(summary(AP_lm)$adj.r.squared,3),
  round(summary(AP_lm)$coefficients[1],3),
  round(summary(AP_lm)$coefficients[2],3),
  round(summary(AP_lm)$coefficients[3],3))

View(lm07_results)

lm07_results <- lm07_results %>%
  rename(
    R_sq = paste(names(lm07_results[1])),
    intercept = paste(names(lm07_results[2])),
    PM25 = paste(names(lm07_results[3])),
    Humidity = paste(names(lm07_results[4])))

View(lm07_results)

# predict for July 

# subset df by extracting sensor pm25 and humidity variables 
pred_data07 <- AP_df_07 %>%
  dplyr::select(pm25, humidity)

# pedict response (monitor pm25)
pred_data07$pred_fit <- predict(AP_lm, newdata= pred_data07)
View(pred_data07)

# plot lm 
par(mfrow=c(1,1))
plot(AP_df_07$pm25_monitor ~ AP_df_07$pm25, col=1, 
     pch=16, main  = "Amazon Park -- July 1-8, 2020", 
     xlab = "Sensor PM2.5 hourly data", 
     ylab = "Monitor PM2.5 hourly data")
abline(AP_lm, col = "red")
lines(pred_data07$pred_fit ~ pred_data07$pm25, col = "blue")


# compare to lm predicted values
lm_predicted <- lm(pred_data07$pred_fit ~ pred_data07$pm25)
summary(lm_predicted)

plot(pred_data07$pred_fit ~ AP_df_07$pm25, col=1, 
     pch=16, main  = "Amazon Park -- July 1-8, 2020")

?predict
?abline

# * run multiple lm w/ humidity -----
AP_mlm <- lm(AP_df_07$pm25_monitor ~ AP_df_07$pm25 + AP_df_07$humidity)

# check mlm results
summary(AP_mlm)

# create a mlm results df
mlm07_results <- data.frame(
  round(summary(AP_mlm)$adj.r.squared,3),
  round(summary(AP_mlm)$coefficients[1],3),
  round(summary(AP_mlm)$coefficients[2],3),
  round(summary(AP_mlm)$coefficients[3],3))

View(mlm07_results)

mlm07_results <- mlm07_results %>%
  rename(
    R_sq = paste(names(mlm07_results[1])),
    intercept = paste(names(mlm07_results[2])),
    PM25 = paste(names(mlm07_results[3])),
    Humidity = paste(names(mlm07_results[4])))

View(mlm07_results)

# pedict response (monitor pm25)
pred_data07$pred_mfit <- predict(AP_mlm, newdata= pred_data07)
View(pred_data07)

par(mfrow=c(2,2))
plot(AP_df_07$pm25_monitor ~ AP_df_07$pm25 + AP_df_07$humidity, col=1, 
     pch=16, main  = "Amazon Park -- July 1-8, 2020", 
     ylab = "Monitor PM2.5 hourly data") 

# * September data frame: sensor and monitor hourly data
# smokiest week according to pat_internalFit() plot 
AP_df_09 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200905,
  enddate = 20200913)
View(AP_df_09)
# note: all columns come from the sensor's pat object exept for "monitor_pm25"

# * run lm ----
AP_lm <- lm(AP_df_09$pm25_monitor ~ AP_df_09$pm25)

# check lm results
summary(AP_lm)

# create a lm results df
lm09_results <- data.frame(
  round(summary(AP_lm)$adj.r.squared,3),
  round(summary(AP_lm)$coefficients[1],3),
  round(summary(AP_lm)$coefficients[2],3),
  round(summary(AP_lm)$coefficients[3],3))

View(lm09_results)

lm09_results <- lm09_results %>%
  rename(
    R_sq = paste(names(lm09_results[1])),
    intercept = paste(names(lm09_results[2])),
    PM25 = paste(names(lm09_results[3])),
    Humidity = paste(names(lm09_results[4])))
   
View(lm09_results)

# plot lm
plot(AP_df_09$monitor_pm25 ~ AP_df_09$pm25, col=1, 
     pch=16, main  = "Amazon Park -- Sep 7-14, 2020", 
     xlab = "Sensor PM2.5 hourly data", 
     ylab = "Monitor PM2.5 hourly data")
abline(AP_lm, col = "red")

# * run multiple lm w/ humidity -----
AP_mlm <- lm(AP_df_09$pm25_monitor ~ AP_df_09$pm25 + AP_df_09$humidity)

# check mlm results 
summary(AP_mlm)

# create a mlm results df
mlm09_results <- data.frame(
  round(summary(AP_mlm)$adj.r.squared,3),
  round(summary(AP_mlm)$coefficients[1],3),
  round(summary(AP_mlm)$coefficients[2],3),
  round(summary(AP_mlm)$coefficients[3],3))

View(mlm09_results)

mlm09_results <- mlm09_results %>%
  rename(
    R_sq = paste(names(mlm09_results[1])),
    intercept = paste(names(mlm09_results[2])),
    PM25 = paste(names(mlm09_results[3])),
    Humidity = paste(names(mlm09_results[4])))

View(mlm09_results)

# plot mlm
par(mfrow=c(2,2))
plot(AP_df_09$pm25_monitor ~ AP_df_09$pm25 + AP_df_09$humidity, col=1, 
     pch=16, main  = "Amazon Park -- Sept 5-13, 2020", 
     ylab = "Monitor PM2.5 hourly data") 





