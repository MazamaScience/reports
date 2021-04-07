################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #1 (MazamaScience/reports)
# Date: 4/7/2021
################################################################

# Local archive for LRAPA pat files 

# Before you start, consider where in your computer you are going to save your data 
# and create your archive. 
# Run `path.expand("~")` in the R console to see where the data will be saved by default. 
# If you wish to create a new data directory, you'll have to specify it in the code
# section `Setup new data directory`, otherwise you can jump to the next R script. 

# ----- Libraries --------------------------------------------------------------
library(MazamaCoreUtils)   
library(AirSensor)
library(PWFSLSmoke)

# ----- Setup new data directory -----------------------------------------------
# check your current home directory 
path.expand("~")

# create a new data directory 
DATA_DIR <- "~/Data/LRAPA" # type yours changing only "~" and keeping "/Data/LRAPA". 
# Example: "C:/Data/LRAPA" 

# LARPA local data archive: Setup

# ----- Setup archive directory ------------------------------------------------
# Create an archive directory underneath ~/Data
if ( exists("DATA_DIR") ) {
  archiveDir <- file.path(DATA_DIR)
} else {
  archiveDir <- file.path("~/Data/LRAPA")
}

dir.create(archiveDir, recursive = TRUE)

# AirSensor package
library(AirSensor)

# Set the archiveBaseUrl so we can get a 'pas' object
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")


# ----- Create PAS object ------------------------------------------------------
# Create a 'pas' object limited to LARPA sensors
#   - load most recent 'pas' for the entire country
#   - subset to include sensors labeled LARPA
LRAPA_sensors <-
  pas_load() %>%
  pas_filter(stringr::str_detect(label, "LRAPA"))

# Look at it
pas_leaflet(LRAPA_sensors)

# Save it in our archive directory
save(LRAPA, file = paste0(archiveDir, "LRAPA_sensors.rda"))

# Examine archive directory:
list.files(file.path(archiveDir))

# ----- Find colocated sensors -------------------------------------------------
# To find colocated senosors check for natural breakpoints in the spatial distribution 
# of the sensors with respect to the closest monitor 

dplyr::filter(LRAPA_sensors, pwfsl_closestDistance < 1000) %>% 
  dplyr::pull(pwfsl_closestDistance) %>% 
  hist(n=100)

# there is a natural breakpoint at 100 m. Let's use that to extract our sensors IDs
near_sensors <- LRAPA_sensors %>%
  pas_filter(pwfsl_closestDistance <= 100)

# there is also a visual way to find the closest sensors
pas_leaflet(LRAPA_sensors, parameter = "pwfsl_closestDistance")

# extract sensors IDs
near_sensorsID <- near_sensors$deviceDeploymentID
near_sensorsID <- unique(near_sensorsID)

# Jon, the reason why I'm not using pas_getDeviceDeploymentIDs(LRAPA_sensors),
# it's becuase it returns a lower number of sensors.
# try 
# LRAPA_sensorsID <- LRAPA_sensors$deviceDeploymentID
# LRAPA_sensorsID <- unique(LRAPA_sensorsID)
# length(LRAPA_sensorsID) # 60
# and then try 
# test_sensorID <- pas_getDeviceDeploymentIDs(LRAPA_sensors)
# length(test_sensorID) # 44

# ----- create pat objects -----------------------------------------------------
# * Setup -----

# Have log messages sent to the console
MazamaCoreUtils::logger.setLevel(TRACE)

# Set the package archiveBaseDir so we can load pat objects with `pat_load()`
setArchiveBaseDir(archiveDir)

# * Get PAS object -----
pas <- get(load(file.path(archiveDir, "LRAPA_sensors.rda")))

# * Prepare PAT info ----- 
# Set up months -- we want data from July to October 
timezone <- "America/Los_Angeles"
monthStamps <- c(202007, 202008, 202009, 202010)

# * Loop over months -----
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
# * Examine archive directory -----
list.files(file.path(archiveDir, "pat/2020/07"))
list.files(file.path(archiveDir, "pat/2020/08"))
list.files(file.path(archiveDir, "pat/2020/09"))
list.files(file.path(archiveDir, "pat/2020/10"))

# notice that the number of sensors ranges from 9 to 13 depending on the months. 


