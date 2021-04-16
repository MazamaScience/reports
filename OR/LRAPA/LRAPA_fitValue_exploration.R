# Author: Astrid Sanna
# Issue: fitValue_exploration #6 (MazamaScience/reports)
# Date: 4/13/2021

library(MazamaCoreUtils)
library(AirSensor)
library(PWFSLSmoke)

source("sensorMonitorData.R")
source("sensorMonitorFit.R")

# ----- load LRAPA sensor and monitirs file from archiveDir --------------------
archiveDir <- "C:/Users/astri/Mirror/Mazamascience/Projects/Data/LRAPA"
LRAPA_sensors <- get(load(file.path(archiveDir, "LRAPA_pas.rda")))
LRAPA_monitors <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))

# ----- load pat file ----------------------------------------------------------
# * setup -----
setArchiveBaseDir(archiveDir)
getArchiveBaseDir()
timezone = "America/Los_Angeles"
monitorID <- "410390060_01"

# * pat -----
Amazon_Park <- pat_load(
  id = "947c72aa269258cc_56971",
  startdate = 20200701,
  enddate = 20201101,
  timezone = timezone)

# ----- data exploration -------------------------------------------------------
# * first week of July -----
# check if under non-smoky conditions the time window affects the fit values
# 1 week window
fitValues07 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200708,
  modelParameters = c("pm25", "humidity"))

print(fitValues07)
#   r.squared intercept  pm25 humidity
# 1     0.978     3.765 0.446    -0.02

# 5 days window
fitValues07_5 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200706,
  modelParameters = c("pm25", "humidity"))

print(fitValues07_5)
#   r.squared intercept  pm25 humidity
# 1     0.979     3.787 0.452   -0.023

# 10  days window
fitValues07_10 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200711,
  modelParameters = c("pm25", "humidity"))

print(fitValues07_10)
#   r.squared intercept  pm25 humidity
# 1     0.976     3.846 0.444    -0.02

# under non-smoky conditions the fit values seem to remain fairly constant


# * smoky week in Sep -----
fitValues09 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200905,
  enddate = 20200912,
  modelParameters = c("pm25", "humidity"))

print(fitValues09)
#   r.squared intercept  pm25 humidity
# 1     0.933   -34.902 0.716    0.496

# ---- Fit Values Timeseries  --------------------------------------------------
# source function fitValueTimeSeries()
fullSeasonFitValues <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 7 # days
) 
View(fullSeasonFitValues)


