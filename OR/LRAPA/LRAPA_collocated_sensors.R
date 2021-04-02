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


#---- libraries ----
library(PWFSLSmoke)
library(AirSensor)

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
length(test_sensorID) #44 less than LRAPA_sensorsID. Why?

#map
pas_leaflet(LRAPA_sensors)

#---- sensors within 1 km from pwfsl_closestDistance 
LRAPA_sensors$pwfsl_closestDistance
near_sensors <- LRAPA_sensors %>%
  pas_filter(pwfsl_closestDistance <= 1000)
length(unique(near_sensors$deviceDeploymentID)) #18 

#---- sensors within 0.5 km from pwfsl_closestDistance 
LRAPA_sensors$pwfsl_closestDistance
near_sensors <- LRAPA_sensors %>%
  pas_filter(pwfsl_closestDistance <= 500)
length(unique(near_sensors$deviceDeploymentID)) #13

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
# select most useful meta data
names(near_sensors)
near_sensors_short <- near_sensors %>%
  select(label, deviceDeploymentID, pwfsl_closestMonitorID, DEVICE_LOCATIONTYPE)


