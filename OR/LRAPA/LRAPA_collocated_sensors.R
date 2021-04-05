################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #1 (MazamaScience/reports)
# Date: 4/2/2021
################################################################

# Request:
# Have a look at the Lane County (Eugene, Oregon area) Air Protection Agency — LRAPA
# 1) How many monitors and how many sensors area in the LRAPA area?
# 2) How many sensors are colocated with FRM monitors?
# 3) For the best colocated sensor, look at the ‘pat’ data and decide whether it 
# is reasonable.
# 3) Process the ‘pat’ data into ‘airsensor’ objects using QC_00 if necessary to 
# retain as much data as is reasonable.
# 4) Use the R lm() function to calculate linear fits of ‘airsensor’ object 
# PM2.5 to ‘monitor’ PM2.5. Do this for one weeks worth of data in each of July, 
# August, September and October.
# 5) See if the model fit parameters differ.
# 6) Now see if you can figure out how to access the hourly aggregated humidity 
# data and then run a multi-variate fit using both PM2.5 and humidity from the sensor.
# 7) Do the model fit parameters change when you add humidity?

#---- answers ----
# 1) monitors = 7; sensors = 60
# 2) 18 sensors pwfsl_closestDistance <= 1000; 
#    13 18 sensors pwfsl_closestDistance <= 500.

#---- Set up ----
# libraries 
library(PWFSLSmoke)
library(AirSensor)

# archiveDir
archiveDir <- file.path("C:/Users/astri/Mirror/Mazamascience/Projects/Data/LRAPA")

#---- LRAPA_monitors -----

LRAPA_monitors <- monitor_loadLatest() %>%
  monitor_subset(stateCodes = "OR")

colnames(LRAPA_monitors$meta)
LRAPA_monitors$meta$siteName
LRAPA_monitors$meta$countyName


# doesn't work 
LRAPA_monitors <- monitor_loadLatest() %>%
  monitor_subset(countyName = "Lane")

# doesn't work
LRAPA_monitors <- monitor_subset(LRAPA_monitors, countyName = "Lane")

# doesn't work
LRAPA_monitors <- monitor_loadLatest() %>%
  monitor_subset(stringr::str_detect(countyName, "Lane"))

# it works
LRAPA_monitors <- monitor_subsetBy(
  LRAPA_monitors, 
  countyName == "Lane")

# number of monitors 
LRAPA_monitorsID <- LRAPA_monitors$meta$monitorID
length(unique(LRAPA_monitorsID)) # 7 monitors 

# map
monitor_leaflet(LRAPA_monitors)

#---- LRAPA_sensors ----
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

LRAPA_sensors <- pas_load() %>%
  pas_filter(stringr::str_detect(label, "LRAPA"))

class(LRAPA_sensors)
LRAPA_sensors$label

# Save it in our archive directory
archiveDir <- file.path("C:/Users/astri/Mirror/Mazamascience/Projects/Data/LRAPA")
save(LRAPA_sensors, file = file.path(archiveDir, "LRAPA_sensors.rda"))

# number of sensors 
LRAPA_sensorsID <- LRAPA_sensors$deviceDeploymentID
LRAPA_sensorsID <- unique(LRAPA_sensorsID)
length(LRAPA_sensorsID)# 60 sensors 

test_sensorID <- pas_getDeviceDeploymentIDs(LRAPA_sensors)
length(test_sensorID) #44 less than LRAPA_sensorsID. Why?**************************

#map
pas_leaflet(LRAPA_sensors)

#---- get colocated sensors -----
# within 1 km from pwfsl_closestDistance 
# LRAPA_sensors$pwfsl_closestDistance
# near_sensors <- LRAPA_sensors %>%
#   pas_filter(pwfsl_closestDistance <= 1000)
# length(unique(near_sensors$deviceDeploymentID)) #18 

# within 0.5 km from pwfsl_closestDistance 
LRAPA_sensors$pwfsl_closestDistance
near_sensors <- LRAPA_sensors %>%
  pas_filter(pwfsl_closestDistance <= 500)
length(unique(near_sensors$deviceDeploymentID)) #13

# select most useful meta data
near_sensors_short <- near_sensors %>%
  select(label, deviceDeploymentID, pwfsl_closestMonitorID, pwfsl_closestDistance, 
         DEVICE_LOCATIONTYPE)

# get near sensor ids
near_sensorsID <- near_sensors$deviceDeploymentID
near_sensorsID <- unique(near_sensorsID)
length(near_sensorsID)

#---- create pat objects ----

library(MazamaCoreUtils)    # for logging
library(AirSensor)

# ----- Setup 

# Have log messages sent to the console
MazamaCoreUtils::logger.setLevel(TRACE)

# Set the package archiveBaseDir so we can load pat objects with `pat_load()`
setArchiveBaseDir(archiveDir)

# ----- Get PAS object

# NOTE:  We won't use the archive directories for the pas object because we
# NOTE:  already have exactly what we want in our `mvcaa.rda` file.

pas <- get(load(file.path(archiveDir, "LRAPA_sensors.rda")))

# ----- Prepare PAT info 
# Set up months
timezone <- "America/Los_Angeles"
monthStamps <- c(202007, 202008, 202009, 202010)

# Loop over months
for ( monthStamp in monthStamps ) {
  
  logger.debug("Working on monthStamp %s ---------- ...", monthStamp) 
  
  # Get POSXct startdate
  startdate <- MazamaCoreUtils::parseDatetime(monthStamp, timezone = timezone)
  
  # Guarantee that the enddate is the first of the next month
  enddate <- lubridate::floor_date(
    startdate + lubridate::ddays(40),
    unit = "month"
  )
  
  # Get YYYY and MM strings
  YYYY <- strftime(startdate, "%Y")
  MM <- strftime(startdate, "%m")
  
  # Initialize counters
  idCount <- length(near_sensorsID)
  count <- 0 
  successCount <- 0
  
  # ----- Create PAT objects 
  
  # Create the archiveDir/pat/YYYY/MM/ directory
  dir.create(
    file.path(archiveDir, "pat", YYYY, MM), 
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  # Loop over all deviceDeploymentIDs
  for ( id in near_sensorsID ) {
    
    # Create PAT canonical file name
    fileName <- paste0("pat_", id, "_", YYYY, MM, ".rda")
    
    # Create PAT canonical file path
    filePath <- file.path(archiveDir, "pat", YYYY, MM, fileName)
    
    count <- count + 1
    logger.debug("Working on %s (%d/%d) ...", id, count, idCount)
    
    # Use a try-block in case you get "no data" errors
    result <- try({
      
      # Create PAT
      pat <- pat_createNew(
        id = id,
        label = NULL,        # not needed if you have the id
        pas = pas,
        startdate = startdate,
        enddate = enddate,
        timezone = timezone,
        baseUrl = "https://api.thingspeak.com/channels/",
        verbose = FALSE
      )
      successCount <- successCount + 1
      save(pat, file = filePath)
      
    }, silent = FALSE)
    
    if ( "try-error" %in% class(result) ) {
      logger.error(geterrmessage())
    }
    
  }
}

#---- analyse pat ----
# load pas file and run again from 101 to 109
LRAPA_sensors <- get(load(file.path(archiveDir, "LRAPA_sensors.rda")))
print(near_sensors_short)

# load colocated pat (500 m) and run analyses
setArchiveBaseDir(archiveDir)
getArchiveBaseDir()

startdate = 20200701
enddate = 20201101
timezone = "America/Los_Angeles"

# Amazon_Park  ************************ ----
Amazon_Park <- pat_load(
  id = "947c72aa269258cc_56971", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Amazon_Park)
pat_scatterPlotMatrix(Amazon_Park)
#h:t = -0.815
# smokiest period: Sep-Oct
AM_lm <- pat_internalFit(Amazon_Park,   showPlot = FALSE)
summary(AM_lm)
# Coefficients:
#           Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) -5.590e-01  9.237e-03   -60.52   <2e-16 ***
# data$pm25_B  1.083e+00  8.154e-05 13286.55   <2e-16 ***
# Adjusted R-squared:  0.9995
AM_lm_ex <- pat_externalFit(Amazon_Park, showPlot = FALSE)
summary(AM_lm_ex)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.866118   0.110348   7.849 6.02e-15 ***
# both_data$pa_pm25 0.632538   0.002273 278.298  < 2e-16 ***
# Adjusted R-squared:  0.9666 

Amazon_Park %>% # took minutes and R got stuck again
  pat_filterDate(20200701, 20200901) %>%
  pat_monitorComparison()


# Bethel ----
Bethel <- pat_load(
  id = "2fe738621b6a062f_10594", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Bethel)
pat_scatterPlotMatrix(Bethel)
#h:t = -0.776
# smokiest period: Sep-Oct
Be_lm <- pat_internalFit(Bethel,   showPlot = FALSE)
summary(Be_lm)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.911e+01  3.795e-01  103.06   <2e-16 ***
# data$pm25_B 5.461e-03  4.383e-04   12.46   <2e-16 ***
# Adjusted R-squared:  0.001867
Be_lm_ex <- pat_externalFit(Bethel, showPlot = FALSE)
summary(Be_lm_ex)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       6.665133   0.384412   17.34   <2e-16 ***
# both_data$pa_pm25 0.132699   0.005912   22.45   <2e-16 ***
# Adjusted R-squared:  0.1789

# Cottage Grove ----
Cot_Grove <- pat_load(
  id = "1f1f1289dc3ba68d_56951", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Cot_Grove)
pat_scatterPlotMatrix(Cot_Grove)
#h:t = -0.825
# smokiest period: Sep-Oct
CG_lm <- pat_internalFit(Cot_Grove,   showPlot = FALSE)
summary(CG_lm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 48.5602458  0.5362219   90.56   <2e-16 ***
# data$pm25_B -0.0035162  0.0003367  -10.44   <2e-16 ***
# Adjusted R-squared:  0.001784 

CG_lm_ex <- pat_externalFit(Cot_Grove, showPlot = FALSE)
summary(CG_lm_ex)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       2.769779   0.171390   16.16   <2e-16 ***
# both_data$pa_pm25 0.594689   0.007919   75.09   <2e-16 ***
# Adjusted R-squared:  0.7763 


# Elgin Ave Outside  115b76a71be4c69d_10608 ----
Elgin_Ave <- pat_load(
  id = "115b76a71be4c69d_10608", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Elgin_Ave)
pat_scatterPlotMatrix(Elgin_Ave)
#h:t = -0.881
# smokiest period: Sep-Oct
EA_lm <- pat_internalFit(Elgin_Ave,   showPlot = FALSE)
summary(EA_lm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 504.11654    4.34725  115.96   <2e-16 ***
# data$pm25_B  -2.02837    0.06335  -32.02   <2e-16 *** 
# Adjusted R-squared:  0.01243

EA_lm_ex <- pat_externalFit(Elgin_Ave, showPlot = FALSE)
summary(EA_lm_ex)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        3.58050    0.09271   38.62   <2e-16 ***
# both_data$pa_pm25  0.34103    0.01156   29.51   <2e-16 ***
# Adjusted R-squared:  0.3484 


# Fairy Glen (months of missing data) ----
Fairy_Glen  <- pat_load(
  id = "ebdef5c81fbd3734_80335", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Fairy_Glen)
pat_scatterPlotMatrix(Fairy_Glen)
#h:t = -0.915
# smokiest period: Sep-Oct
FG_lm <- pat_internalFit(Fairy_Glen,   showPlot = FALSE)
summary(FG_lm)
# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.6412     3.1768   0.831    0.408    
# data$pm25_B   0.9431     0.0163  57.853   <2e-16 ***
# Adjusted R-squared:  0.9693 

FG_lm_ex <- pat_externalFit(Fairy_Glen, showPlot = FALSE)
summary(FG_lm_ex)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
# (Intercept)        8.66971    1.36421   6.355  0.00314 **
# both_data$pa_pm25 -0.02843    0.01013  -2.806  0.04853 * 
# Adjusted R-squared:  0.5789 



# Madison MS ----
Madison  <- pat_load(
  id = "41e41572d3d6161b_3413", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Madison)
pat_scatterPlotMatrix(Madison)
#h:t = -0.774
# smokiest period: Sep-Oct
Ma_lm <- pat_internalFit(Madison,   showPlot = FALSE)
summary(Ma_lm)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 31.93566    0.54988   58.08   <2e-16 ***
# data$pm25_B  0.67337    0.00324  207.81   <2e-16 ***
# Adjusted R-squared:  0.5042

Ma_lm_ex <- pat_externalFit(Madison, showPlot = FALSE)
summary(Ma_lm_ex)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       7.283138   0.296977   24.52   <2e-16 ***
# both_data$pa_pm25 0.203559   0.008988   22.65   <2e-16 ***
# Adjusted R-squared:  0.2395


