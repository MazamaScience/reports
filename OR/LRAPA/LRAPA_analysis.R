################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #1 (MazamaScience/reports)
# Date: 4/8/2021
################################################################
# Now that we have found colocate sensors (within 100 m from the closest FRM 
# monitor) we can start exploring the pat files by running pat_scatterPlotMatrix()
# pat_internalFit and pat_externalFit, and select the best performing sensors.

# ----- Libraries --------------------------------------------------------------
library(MazamaCoreUtils)   
library(AirSensor)
library(PWFSLSmoke)

# ----- Create a table including only $meta useful to your analysis ------------
# load LRAPA sensor file from archiveDir 
LRAPA_sensors <- get(load(file.path(archiveDir, "LRAPA_sensors.rda")))

# create table 
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

# ----- Linear moled PM2.5 monitor ~ PM2.5 sensor for specific weeks -----------
# * Load monitors 
LRAPA_monitors <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))

# * July: PM2.5 hourly data (HD) for the Amazon Park MONITOR -----
AP_monitor_HD <-
  LRAPA_monitors %>% 
  monitor_subset(monitorIDs = "410390060_01", tlim=c(20200710, 20200718)) %>% 
  monitor_extractData() %>% 
  dplyr::rename(monitor_pm25 = "410390060_01")

names(AP_monitor_HD) # double check columns names 

# * July: PM2.5 HD for the Amazon Park SENSOR -----
AP_sensor_HD <-
  Amazon_Park %>%
  pat_filterDate(20200710, 20200718, timezone = timezone) %>% 
  pat_aggregate() %>%
  pat_extractData() %>% 
  dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
  dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity)

names(AP_sensor_HD) # double check columns names 

# * combine monitor and sensor data ----
AP_comb_07 <- dplyr::left_join(AP_sensor_HD, AP_monitor_HD, by = "datetime")
names(AP_comb_07) #check columns 
# note: all columns come from the sensor's pat object exept for "monitor_pm25"

# * run lm ----
AP_lm <- lm(AP_comb_07$monitor_pm25 ~ AP_comb_07$pm25)
summary(AP_lm)
plot(AP_comb_07$monitor_pm25 ~ AP_comb_07$pm25, col=1, 
     pch=16, main  = "Amazon Park -- July 10-18, 2020", 
     xlab = "Sensor PM2.5 hourly data", 
     ylab = "Monitor PM2.5 hourly data")
abline(AP_lm, col = "red")
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      3.32711    0.06316   52.68   <2e-16 ***
# AP_comb_07$pm25  0.38453    0.01105   34.81   <2e-16 ***

# Adjusted R-squared:  0.8675 

# * run multiple lm w/ humidity -----
AP_mlm <- lm(AP_comb_07$monitor_pm25 ~ AP_comb_07$pm25 + AP_comb_07$humidity)
summary(AP_mlm)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.981280   0.076219   52.23   <2e-16 ***
# AP_comb_07$pm25      0.422325   0.009184   45.98   <2e-16 ***
# AP_comb_07$humidity -0.018184   0.001627  -11.18   <2e-16 ***

# Adjusted R-squared:  0.9208 

plot(AP_comb_07$monitor_pm25 ~ AP_comb_07$pm25 + AP_comb_07$humidity, col=1, 
     pch=16, main  = "Amazon Park -- July 10-18, 2020", 
     xlab = "Sensor hourly data", 
     ylab = "Monitor PM2.5 hourly data") 


AP_fit<- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200710,
  enddate = 20200718,
  modelParameters = c("pm25", "humidity")
)
# Error in MazamaCoreUtils::parseDatetime(tlim, timezone = timezone) : 
# argument 'timezone' must be a character string of length one
# Called from: MazamaCoreUtils::parseDatetime(tlim, timezone = timezone)

# * September: PM2.5 hourly data (HD) for the Amazon Park MONITOR -----
AP_monitor_HD <-
  LRAPA_monitors %>% 
  monitor_subset(monitorIDs = "410390060_01", tlim=c(20200907, 20200915)) %>% 
  monitor_extractData() %>% 
  dplyr::rename(monitor_pm25 = "410390060_01")

names(AP_monitor_HD) # double check columns names 

# * September: PM2.5 HD for the Amazon Park SENSOR -----
AP_sensor_HD <-
  Amazon_Park %>%
  pat_filterDate(20200907, 20200915, timezone = timezone) %>% 
  pat_aggregate() %>%
  pat_extractData() %>% 
  dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
  dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity)

names(AP_sensor_HD) # double check columns names 

# * combine monitor and sensor data ----
AP_comb_09 <- dplyr::left_join(AP_sensor_HD, AP_monitor_HD, by = "datetime")
names(AP_comb_08) #check columns 
# note: all columns come from the sensor's pat object exept for "monitor_pm25"

# * run lm ----
AP_lm <- lm(AP_comb_09$monitor_pm25 ~ AP_comb_09$pm25)
summary(AP_lm)
plot(AP_comb_09$monitor_pm25 ~ AP_comb_09$pm25, col=1, 
     pch=16, main  = "Amazon Park -- Sep 7-14, 2020", 
     xlab = "Sensor PM2.5 hourly data", 
     ylab = "Monitor PM2.5 hourly data")
abline(AP_lm, col = "red")
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -35.87032    9.78607  -3.665 0.000325 ***
# AP_comb_08$pm25   0.82800    0.02272  36.437  < 2e-16 ***

# Adjusted R-squared:  0.8799

# * run multiple lm w/ humidity -----
AP_mlm <- lm(AP_comb_09$monitor_pm25 ~ AP_comb_09$pm25 + AP_comb_09$humidity)
summary(AP_mlm)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -82.52439   11.78868  -7.000 4.95e-11 ***
# AP_comb_08$pm25       0.80797    0.02101  38.451  < 2e-16 ***
# AP_comb_08$humidity   1.13133    0.18642   6.069 7.51e-09 ***

# Adjusted R-squared:  0.8999 

plot(AP_comb_09$monitor_pm25 ~ AP_comb_09$pm25 + AP_comb_09$humidity, col=1, 
     pch=16, main  = "Amazon Park -- Sept 7-14, 2020", 
     xlab = "Sensor hourly data", 
     ylab = "Monitor PM2.5 hourly data") 




