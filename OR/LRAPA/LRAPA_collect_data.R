################################################################################
# Author: Astrid Sanna
# Issue: LRAPA co-located sensors #1 (MazamaScience/reports)
# Date: 4/7/2021
################################################################################

# Local archive for LRAPA pat files

# Before you start, consider where in your computer you are going to save your
# data and create your archive. Run `path.expand("~")` in the R console to see
# where the data will be saved by default. If you wish to create a new data
# directory, you'll have to specify it in the code section `Setup new data
# directory`, otherwise you can jump to the next R script.

# ----- Libraries --------------------------------------------------------------

library(MazamaCoreUtils)
library(PWFSLSmoke)
library(AirSensor)

# ----- Setup LRAPA data directory ---------------------------------------------

# Remove any previously set archiveBaseDir
setArchiveBaseDir(NULL)

# Check your current home directory
path.expand("~")

# Use the default archiveDir unless it is already defined
if ( !exists("archiveDir") ) {
  archiveDir <- file.path("~/Data/LRAPA")
}

dir.create(archiveDir, recursive = TRUE, showWarnings = FALSE)

# Set the archiveBaseUrl so we can get a pre-generated 'pas' object
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

# ----- Create/save LRAPA pas object -------------------------------------------

# Create a 'pas' object limited to LARPA sensors
#   - load most recent 'pas' for the entire country
#   - filter to include sensors labeled LARPA
LRAPA_pas <-
  pas_load() %>%
  pas_filter(stringr::str_detect(label, "LRAPA"))

# Look at it
pas_leaflet(LRAPA_pas)

# Save it in our archive directory
save(LRAPA_pas, file = paste0(archiveDir, "LRAPA_pas.rda"))

# Examine archive directory:
list.files(file.path(archiveDir))

# ----- Find co-located sensor/monitor pairs -----------------------------------

# To find co-located sensors, check a natural breakpoint in the "closest distance"
# parameter.
dplyr::filter(LRAPA_pas, pwfsl_closestDistance < 1000) %>%
  dplyr::pull(pwfsl_closestDistance) %>%
  hist(n = 100)

# There is a natural breakpoint at 100 m.

# * Extract sensors IDs w/in 100 m radius from nearest monitor
paired_pas <-
  LRAPA_pas %>%
  pas_filter(pwfsl_closestDistance <= 100)

# There is also a visual way to find the closest sensors one at a time
pas_leaflet(LRAPA_pas, parameter = "pwfsl_closestDistance")

# Extract sensors IDs
pairedSensorIDs <- pas_getDeviceDeploymentIDs(paired_pas)

# ----- Load/save monitor data -------------------------------------------------

# * Extract monitor IDs
pairedMonitorIDs <- unique(paired_pas$pwfsl_closestMonitorID)

# * Load monitors data for July-October 2020
LRAPA_monitors <-
  monitor_load(startdate = 20200701, enddate = 20201101) %>%
  monitor_subset(monitorIDs = pairedMonitorIDs)

# * Save monitors data in the archive directory
save(LRAPA_monitors, file = file.path(archiveDir, "LRAPA_monitors.rda"))

# ----- Create pat archive -----------------------------------------------------

# * Setup -----

# Have log messages sent to the console
MazamaCoreUtils::logger.setLevel(TRACE)

# Set the package archiveBaseDir so we can load pat objects with `pat_load()`
setArchiveBaseDir(archiveDir)
getArchiveBaseDir() # double check

# Set up months -- we want data from July to October
timezone <- "America/Los_Angeles"
monthStamps <- c(202007, 202008, 202009, 202010)

# * Loop over months -----

for ( monthStamp in monthStamps ) {

  logger.debug("Working on monthStamp %s ---------- ...", monthStamp)

  # Get POSIXct startdate
  startdate <- MazamaCoreUtils::parseDatetime(monthStamp, timezone = timezone)

  # Guarantee that the enddate is the first of the next month (at midnight)
  enddate <- lubridate::floor_date(
    startdate + lubridate::ddays(40),
    unit = "month"
  )

  # Get YYYY and MM strings
  YYYY <- strftime(startdate, "%Y")
  MM <- strftime(startdate, "%m")

  # Initialize counters
  idCount <- length(pairedSensorIDs)
  count <- 0
  successCount <- 0

  # Create the archiveDir/pat/YYYY/MM/ directory
  dir.create(
    file.path(archiveDir, "pat", YYYY, MM),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # ** Loop over deviceDeploymentIDs -----

  for ( id in pairedSensorIDs ) {

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
        pas = paired_pas,
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

  } # END of (id in pariedSensorIDs)

} # END of (monthStamp in monthSstamps)

# * Examine archive directories -----

list.files(file.path(archiveDir, "pat/2020/07"))
list.files(file.path(archiveDir, "pat/2020/08"))
list.files(file.path(archiveDir, "pat/2020/09"))
list.files(file.path(archiveDir, "pat/2020/10"))

# Notice that the number of sensors ranges from 7 to 8 depending on the month.




