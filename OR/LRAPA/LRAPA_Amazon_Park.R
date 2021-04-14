################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #4 (MazamaScience/reports)
# Date: 4/13/2021
################################################################
# Now that we have found colocate sensors (within 100 m from the closest FRM 
# monitor) we can start exploring the pat files by running pat_scatterPlotMatrix()
# pat_internalFit and pat_externalFit, and select the best performing sensors. 
# In this case it will turn out to be the Amazon Park sensor.
# We'll then:
# 1) combine sensor hourly data (PM2.5, temperature, humidity) with monitor
# hourly data (PM2.5) using the sensorMonitorData() function.
# 2) fit linear and multilinear models for one week of low and one week of high
# PM2.5 concentrations to explore the influence of humidity on the 
# sensor performance.
# 3) predict monitor PM25 data.
# 4) create timeseries including raw and fitted data.


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
# * setup -----
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
Amazon_Park %>% 
  pat_filterDate(20200905, 20200913) %>%
  pat_multiplot()


# reapeat this step for other sensors you want to explore 

# ----- Explore pat data -------------------------------------------------------
pat_multiplot(Amazon_Park)
pat_scatterPlotMatrix(Amazon_Park)
lm <- pat_internalFit(Amazon_Park, showPlot = TRUE)
summary(lm)
lm_ex <- pat_externalFit(Amazon_Park, showPlot = TRUE)
summary(lm_ex)

# reapeat this step for other sensors you want to explore 

# ----- Linear model PM2.5 monitor ~ PM2.5 sensor for specific weeks -----------
# We'll be using the function sensorMonitorData() to create a data frame (df) 
# containing the raw data we'll need to build our linear and multilinear models.

# * Setup sensorMonitorData() 
# 1) Open LRAPA_utils.R script 
# 2) "Source" the script 
# 3) Check that you have sensorMonitorData() in your R environment 

# * July 01-08 data frame: sensor and monitor hourly data -----
# low PM25 week according to pat_internalFit() plot  
df_07<- sensorMonitorData(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200701,
  enddate = 20200708)
View(df_07)
# note: all columns come from the sensor's pat object exept for "pm25_monitor"

# * run lm ----
lm <- lm(df_07$pm25_monitor ~ df_07$pm25)

# check lm results
summary(lm)

# create a lm results df
lm07_results <- data.frame(
  round(summary(lm)$adj.r.squared,3),
  round(summary(lm)$coefficients[1],3),
  round(summary(lm)$coefficients[2],3))
View(lm07_results)

lm07_results <- lm07_results %>%
  rename(
    R_sq = paste(names(lm07_results[1])),
    intercept = paste(names(lm07_results[2])),
    PM25 = paste(names(lm07_results[3])))
View(lm07_results)

# * run multiple lm w/ humidity -----
mlm <- lm(df_07$pm25_monitor ~ df_07$pm25 + df_07$humidity)

# check mlm results
summary(mlm)

# create a mlm results df
mlm07_results <- data.frame(
  round(summary(mlm)$adj.r.squared,3),
  round(summary(mlm)$coefficients[1],3),
  round(summary(mlm)$coefficients[2],3),
  round(summary(mlm)$coefficients[3],3))
View(mlm07_results)

mlm07_results <- mlm07_results %>%
  rename(
    R_sq = paste(names(mlm07_results[1])),
    intercept = paste(names(mlm07_results[2])),
    PM25 = paste(names(mlm07_results[3])),
    Humidity = paste(names(mlm07_results[4])))
View(mlm07_results)

# * predict July monitor PM2.5 ---------------------------------------------
# subset df_07 by extracting sensor pm25 and humidity variables 
pred_data07 <- df_07 %>%
  dplyr::select(pm25, humidity)

# pedict lm response (monitor pm25)
pred_data07$pred_pm25_monitor <- predict(lm, newdata= pred_data07)
View(pred_data07)

# pedict mlm response (monitor pm25)
pred_data07$pred_pm25_monitor_humidity <- predict(mlm, newdata= pred_data07)
View(pred_data07)

# * September 05-13 data frame: sensor and monitor hourly data -----
# smokiest week according to pat_internalFit() plot 
df_09 <- sensorMonitorData(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200905,
  enddate = 20200913)
View(df_09)
# note: all columns come from the sensor's pat object exept for "pm25_monitor"

# * run lm ----
lm <- lm(df_09$pm25_monitor ~ df_09$pm25)

# check lm results
summary(lm)

# create a lm results df
lm09_results <- data.frame(
  round(summary(lm)$adj.r.squared,3),
  round(summary(lm)$coefficients[1],3),
  round(summary(lm)$coefficients[2],3))
View(lm09_results)

lm09_results <- lm09_results %>%
  rename(
    R_sq = paste(names(lm09_results[1])),
    intercept = paste(names(lm09_results[2])),
    PM25 = paste(names(lm09_results[3])))
View(lm09_results)

# * run multiple lm w/ humidity -----
mlm <- lm(df_09$pm25_monitor ~ df_09$pm25 + df_09$humidity)

# check mlm results 
summary(mlm)

# create a mlm results df
mlm09_results <- data.frame(
  round(summary(mlm)$adj.r.squared,3),
  round(summary(mlm)$coefficients[1],3),
  round(summary(mlm)$coefficients[2],3),
  round(summary(mlm)$coefficients[3],3))
View(mlm09_results)

mlm09_results <- mlm09_results %>%
  rename(
    R_sq = paste(names(mlm09_results[1])),
    intercept = paste(names(mlm09_results[2])),
    PM25 = paste(names(mlm09_results[3])),
    Humidity = paste(names(mlm09_results[4])))
View(mlm09_results)

# * predict September monitor PM2.5 -----
# subset df_09 by extracting sensor pm25 and humidity variables 
pred_data09 <- df_09 %>%
  dplyr::select(pm25, humidity)

# pedict lm response (monitor pm25)
pred_data09$pred_pm25_monitor <- predict(lm, newdata= pred_data09)
View(pred_data09)

# pedict mlm response (monitor pm25)
pred_data09$pred_pm25_monitor_humidity <- predict(mlm, newdata= pred_data09)
View(pred_data09)

# ----- Timeseries -------------------------------------------------------------
# * July -----
# create a single df including the the fitted monitor values 
df_07 <- df_07 %>%
  left_join(pred_data07)
df_07$pred_pm25_monitor <- round(df_07$pred_pm25_monitor)
df_07$pred_pm25_monitor_humidity <- round(df_07$pred_pm25_monitor_humidity)
View(df_07)

# * create timeseries 
library(ggplot2)
gg07 <-
  ggplot(df_07) +
  geom_line(aes(x = datetime, y = pred_pm25_monitor_humidity), color = "blue") +
  geom_line(aes(x = datetime, y = pm25_monitor), linetype = "dashed") +
  geom_point(aes(x = datetime, y = pm25 ), shape = 1) +
  ylab("PM2.5 (µg/m3)") +
  xlab("Date") +
  ggtitle("Raw and Fitted Monitor Data -- July 01-08, 2020")
print(gg07)

# * September -----
# create a single df including the the fitted monitor values 
df_09 <- df_09 %>%
  left_join(pred_data09)
df_09$pred_pm25_monitor<- round(df_09$pred_pm25_monitor)
df_09$pred_pm25_monitor_humidity <- round(df_09$pred_pm25_monitor_humidity)
df_09$pm25_monitor<- round(df_09$pm25_monitor)
View(df_09)

#* create timeseries 
gg09 <-
  ggplot(df_09) +
  geom_line(aes(x = datetime, y = pred_pm25_monitor_humidity), color = "blue") +
  geom_line(aes(x = datetime, y = pm25_monitor), linetype = "dashed") +
  geom_point(aes(x = datetime, y = pm25 ), shape = 1) +
  ylab("PM2.5 (µg/m3)") +
  xlab("Date") +
  ggtitle("Raw and Fitted Monitor Data -- Sep 05-12, 2020")
print(gg09)
View


