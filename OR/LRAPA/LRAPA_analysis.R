################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #1 (MazamaScience/reports)
# Date: 4/7/2021
################################################################
# Now that we have found colocate sensors (within 100 m from the closest FRM 
# monitor) we can start exploring the pat files by running pat_scatterPlotMatrix()
# pat_internalFit and pat_externalFit, and select the best performing sensors.

# ----- Create a table including only info useful for your analysis ------------
near_sensors_tb <- near_sensors %>%
  dplyr::select(label, 
         deviceDeploymentID, 
         pwfsl_closestMonitorID, 
         pwfsl_closestDistance, 
         DEVICE_LOCATIONTYPE)

# ----- Load and explore pat data ----------------------------------------------
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

pat_multiplot(Amazon_Park)
pat_scatterPlotMatrix(Amazon_Park)
AM_lm <- pat_internalFit(Amazon_Park, showPlot = TRUE)
summary(AM_lm)
AM_lm_ex <- pat_externalFit(Amazon_Park, showPlot = TRUE)
summary(AM_lm_ex)


# * other sensor -----
# * other sensor -----

# ----- Linear moled PM2.5 monitor ~ PM2.5 sensor for specific weeks
# * Load monitors 
LRAPA_monitors <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))

# * Extract PM2.5 hourly data (HD) for the Amazon Park MONITOR ----- 

AP_monitor_HD <-
  LRAPA_monitors %>% 
  monitor_subset(monitorIDs = "410390060_01", tlim=c(20200710, 20200718)) %>% 
  monitor_extractData() %>% 
  dplyr::rename(monitor_pm25 = "410390060_01")

names(AP_monitor_HD) # double check columns names 

# * Extract PM2.5 HD for the Amazon Park SENSOR -----
AP_sensor_HD <-
  Amazon_Park %>%
  pat_filterDate(20200710, 20200718, timezone = timezone) %>% 
  pat_aggregate() %>%
  pat_extractData() %>% 
  dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
  dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity)

names(AP_sensor_HD) # double check columns names 

# run lm analysis -- left here 
