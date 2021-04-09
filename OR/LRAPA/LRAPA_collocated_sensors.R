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
# 3) reasonable data for Amazon Park, Oakridge 2, Springfield City Hall, 
# Oakridge 3 (startdate = 20200815)

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

# Save it in your archive directory
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

# visual way to get closest sensors 
pas_leaflet(LRAPA_sensors, parameter = "pwfsl_closestDistance")

# code to find natural breakpoint to define "colocated"
dplyr::filter(LRAPA_sensors, pwfsl_closestDistance < 1000) %>% 
  dplyr::pull(pwfsl_closestDistance) %>% 
  hist(n=100)

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
AM_lm <- pat_internalFit(Amazon_Park, showPlot = TRUE)
summary(AM_lm)
# Coefficients:
#           Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) -5.590e-01  9.237e-03   -60.52   <2e-16 ***
# data$pm25_B  1.083e+00  8.154e-05 13286.55   <2e-16 ***
# Adjusted R-squared:  0.9995
AM_lm_ex <- pat_externalFit(Amazon_Park, showPlot = TRUE)
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


# Elgin Ave Outside ----
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

# Oakridge 1	 ----
Oakridge1 <- pat_load(
  id = "fd5f069cc7447626_56793", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Oakridge1)
pat_scatterPlotMatrix(Oakridge1)
#h:t = -0.850
# smokiest period: Sep-Oct
Oa1_lm <- pat_internalFit(Oakridge1,   showPlot = FALSE)
summary(Oa1_lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 386.07381    4.35050   88.74   <2e-16 ***
# data$pm25_B  -1.38365    0.04509  -30.68   <2e-16 ***
# Adjusted R-squared:  0.01514 
Oa1_lm_ex <- pat_externalFit(Oakridge1, showPlot = FALSE)
summary(Oa1_lm_ex)
# try remove august
Oakridge1 %>% 
  pat_filterDate(20200901, 20201101) %>%
  pat_multiplot()
Oakridge1 %>% 
  pat_filterDate(20200901, 20201101) %>%
  pat_scatterPlotMatrix()
# bad data 

# Oakridge 2	*********************************** ----
Oakridge2 <- pat_load(
  id = "f2ace631a501333b_38681", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Oakridge2)
pat_scatterPlotMatrix(Oakridge2)
#h:t = -0.880
# smokiest period: Sep-Oct
Oa2_lm <- pat_internalFit(Oakridge2,   showPlot = TRUE)
summary(Oa2_lm)
# Coefficients:
#               Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 4.840e-01  7.971e-03    60.72   <2e-16 ***
# data$pm25_B 9.666e-01  8.447e-05 11442.67   <2e-16 ***
# Adjusted R-squared:  0.9994 
Oa2_lm_ex <- pat_externalFit(Oakridge2, showPlot = TRUE)
summary(Oa2_lm_ex)
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.514244   0.178614  -8.478   <2e-16 ***
# both_data$pa_pm25  0.824115   0.002476 332.855   <2e-16 ***
# Adjusted R-squared:  0.9752 



# Oakridge 3 (startdate = 20200815) *********************** ----
Oakridge3 <- pat_load(
  id = "799da69adac45e75_38631", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Oakridge3)
# try remove july till 14 august 
Oakridge3 %>% pat_filterDate(20200815, 20201101)%>%
  pat_scatterPlotMatrix()
#h:t = -0.844

# create new pat without bad data 
Oakridge3 <- pat_load(
  id = "799da69adac45e75_38631", 
  startdate = 20200815, 
  enddate = enddate, 
  timezone = timezone)

# smokiest period: Sep-Oct
Oa3_lm <- pat_internalFit(Oakridge3,   showPlot = FALSE)
summary(Oa3_lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.4246264  0.0125038  -33.96   <2e-16 ***
# data$pm25_B  1.1337854  0.0001136 9978.65   <2e-16 ***
# Adjusted R-squared:  0.9995 
Oa3_lm_ex <- pat_externalFit(Oakridge3, showPlot = TRUE)
summary(Oa3_lm_ex)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.393017   0.159202   -8.75   <2e-16 ***
# both_data$pa_pm25  0.769792   0.005134  149.94   <2e-16 ***
# Adjusted R-squared:  0.9329 

# Springfield City Hall ********************** -----
Springfield <- pat_load(
  id = "12426967f1fc742c_10606", 
  startdate = startdate, 
  enddate = enddate, 
  timezone = timezone)

pat_multiplot(Springfield)
pat_scatterPlotMatrix(Springfield)
#h:t = -0.738
# smokiest period: Sep-Oct
SC_lm <- pat_internalFit(Springfield,   showPlot = FALSE)
summary(SC_lm)
# Coefficients:
#              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 0.0013514  0.0138823    0.097    0.922    
# data$pm25_B 0.9827831  0.0001008 9750.023   <2e-16 ***
# Adjusted R-squared:  0.9992 
SC_lm_ex <- pat_externalFit(Springfield, showPlot = FALSE)
summary(SC_lm_ex)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1.424547   0.495640   2.874  0.00408 ** 
# both_data$pa_pm25 0.661990   0.004622 143.238  < 2e-16 ***
# Adjusted R-squared: 0.8783

#----- create airsensor objects param PM2.5 ----
# Prepare sensor info 
# Set up months
timezone <- "America/Los_Angeles"
monthStamps <- c(202007, 202008, 202009, 202010)

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
  # Create the archiveDir/airsensor/YYYY/ directory
  dir.create(
    file.path(archiveDir, "airsensor", YYYY), 
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  # Assign a collection name that makes sense
  collectionName <- "lrapa"
  
  # Init counts
  successCount <- 0
  count <- 0
  
  dataList <- list()
  
  # Loop over all ids and aggregate to hourly
  for ( id in near_sensorsID ) {
    
    count <- count + 1
    
    # Debug info
    logger.debug(
      "%4d/%d Calling pat_createAirSensor('%s')",
      count,
      length(near_sensorsID),
      id
    )
    
    # Load the pat data, convert to an airsensor and add to dataList
    dataList[[id]] <- tryCatch(
      expr = {
        airsensor <- pat_load(
          id = id,
          label = NULL,
          pas = LRAPA_sensors,
          startdate = startdate,
          enddate = enddate,
          timezone = "America/Los_Angeles"
        ) %>%
          pat_createAirSensor(
            FUN = AirSensor::PurpleAirQC_hourly_AB_00
          )
      }, 
      error = function(e) {
        logger.warn('Unable to load PAT data for %s ', id)
        NULL
      }
      
      # Keep going in the face of errors
    )
    
  } # END of deviceDeploymentIDs loop
  
  # Combine the airsensors into a single airsensor object and save
  tryCatch(
    expr = {
      logger.info('Combining airsensors...')
      
      airsensor <- PWFSLSmoke::monitor_combine(dataList)
      class(airsensor) <- c("airsensor", "ws_monitor", "list")
      
      logger.info('Combined successfully...')
      
      # Create Airsensor canonical file name
      fileName <- paste0("airsensor_", collectionName, "_", YYYY, MM, ".rda")
      
      # Create Airsensor canonical file path
      filePath <- file.path(archiveDir, "airsensor", YYYY, fileName)
      
      save(list = "airsensor", file = filePath)
    }, 
    error = function(e) {
      msg <- paste("Error creating monthly AirSensor file: ", e)
      logger.error(msg)
    }
  )
  
  # Now proceed to the next month
}

#----- create airsensor objects param humidity WIP ----
# Prepare sensor info 
# Set up months
timezone <- "America/Los_Angeles"
monthStamps <- c(202007, 202008, 202009, 202010)

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
  # Create the archiveDir/airsensor/YYYY/ directory
  dir.create(
    file.path(archiveDir, "airsensor", YYYY), 
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  # Assign a collection name that makes sense
  collectionName <- "lrapa"
  
  # Init counts
  successCount <- 0
  count <- 0
  
  dataList <- list()
  
  # Loop over all ids and aggregate to hourly
  for ( id in near_sensorsID ) {
    
    count <- count + 1
    
    # Debug info
    logger.debug(
      "%4d/%d Calling pat_createAirSensor('%s')",
      count,
      length(near_sensorsID),
      id
    )
    
    # Load the pat data, convert to an airsensor and add to dataList
    dataList[[id]] <- tryCatch(
      expr = {
        airsensor <- pat_load(
          id = id,
          label = NULL,
          pas = LRAPA_sensors,
          startdate = startdate,
          enddate = enddate,
          timezone = "America/Los_Angeles"
        ) %>%
          pat_createAirSensor(
            FUN = AirSensor::PurpleAirQC_hourly_AB_00
          )
      }, 
      error = function(e) {
        logger.warn('Unable to load PAT data for %s ', id)
        NULL
      }
      
      # Keep going in the face of errors
    )
    
  } # END of deviceDeploymentIDs loop
  
  # Combine the airsensors into a single airsensor object and save
  tryCatch(
    expr = {
      logger.info('Combining airsensors...')
      
      airsensor <- PWFSLSmoke::monitor_combine(dataList)
      class(airsensor) <- c("airsensor", "ws_monitor", "list")
      
      logger.info('Combined successfully...')
      
      # Create Airsensor canonical file name
      fileName <- paste0("airsensor_", collectionName, "_", YYYY, MM, ".rda")
      
      # Create Airsensor canonical file path
      filePath <- file.path(archiveDir, "airsensor", YYYY, fileName)
      
      save(list = "airsensor", file = filePath)
    }, 
    error = function(e) {
      msg <- paste("Error creating monthly AirSensor file: ", e)
      logger.error(msg)
    }
  )
  
  # Now proceed to the next month
}

#----- airsensor analysis ----
All_sensors <- sensor_load(
  collection = "lrapa",
  startdate = startdate,
  enddate = enddate,
  timezone = timezone
)
monitor_timeseriesPlot(All_sensor)
All_sensors_meta <- All_sensors$meta

# Amazon Park sensor 
AP_sensor <- monitor_subset(
  All_sensors,
  monitorIDs = 	"947c72aa269258cc_56971",
  dropMonitors = TRUE,
  timezone = timezone
)
monitor_timeseriesPlot(AP_sensor)

AP_sensor %>% sensor_calendarPlot()

# Oakridge 2 
Oa_sensor <- monitor_subset(
  All_sensors,
  monitorIDs = 	"f2ace631a501333b_38681",
  dropMonitors = TRUE,
  timezone = timezone
)
monitor_timeseriesPlot(Oa_sensor)

Oa_sensor %>% sensor_calendarPlot()

# Oakridge 3 
Oa3_sensor <- monitor_subset(
  All_sensors,
  monitorIDs = 	"799da69adac45e75_38631",
  dropMonitors = TRUE,
  timezone = timezone
)
monitor_timeseriesPlot(Oa3_sensor)

Oa3_sensor %>% sensor_calendarPlot()

# Springfield City Hall 
SC_sensor <- monitor_subset(
  All_sensors,
  monitorIDs = 	"12426967f1fc742c_10606",
  dropMonitors = TRUE,
  timezone = timezone
)
monitor_timeseriesPlot(SC_sensor)
SC_sensor %>% sensor_calendarPlot()

#----- figuring out humidity WIP ----
# left here: I guess I'll have to create separate sensor objects, one per parameter.
# extract data 
AP_sensor_d <- AP_sensor %>% 
  sensor_extractData()

test <- pat_createAirSensor(
  pat = Amazon_Park,
  parameter = "humidity",
  FUN = NULL)
test_data <- test$data
?pat_createAirSensor

# Error: Can't subset columns that don't exist.
# x Column `humidity` doesn't exist.
# Run `rlang::last_error()` to see where the error occurred.
# In addition: Warning message:
# In if (!parameter %in% names(hourlyData)) { :
#   the condition has length > 1 and only the first element will be used


# try create function for humidity 
# Custom FUN
humidity <- function(pat, humidity = hourlyData$humidity) {
  
  # Default hourly aggregation
  hourlyData <- 
    pat %>%
    pat_aggregate() %>%
    pat_extractData()
  
  # Create custom_pm variable 
  humidity <- hourlyData$humidity
  
  return(hourlyData)
} 

# Evaluate custom FUN 
sensor <- pat_createAirSensor(
  Amazon_Park, 
  parameter = "humidity", 
  FUN = humidity
)

sensor_data <- sensor$data


#----- sensorPM25 ~ monitorPM25 ----
# 4) Use the R lm() function to calculate linear fits of ‘airsensor’ object 
# PM2.5 to ‘monitor’ PM2.5. Do this for one weeks worth of data in each of July, 
# August, September and October.

# load monitor data for 2020
LRAPA_monitors2020 <- monitor_load(startdate = 20200701, enddate = 20201101) %>%
  monitor_subset(monitorIDs = c("410390060_01", "410392013_01", "410391009_01"))
monitors_meta <- LRAPA_monitors2020$meta

##################### Amazon Park ##############################################
# create Amazon Park monitor obj
AP_monitor <- LRAPA_monitors2020 %>%
  monitor_subset(monitorIDs = "410390060_01")
AP_monitor_data <- AP_monitor$data # discrete values

# combine sensor and monitor
AP_combine <- list(AP_sensor, AP_monitor)
AP_combine <- monitor_combine(AP_combine)

AP_sensor %>% sensor_calendarPlot()
# extract July week 
AP_combine_data <- AP_combine$data
AP_comb_07 <- AP_combine_data %>%
  monitor_subsetData(
    tlim = c(20200710, 20200718),
    timezone = timezone
  )

# run lm monitor~sensor
AP07_lm <- lm(AP_comb_07$`410390060_01` ~ AP_comb_07$`947c72aa269258cc_56971`)
summary(AP07_lm)
par(mfrow=c(2,2))
plot(AP07_lm)
library(car)
shapiro.test(AP07_lm$residuals) #p-value = 0.09393
ncvTest(AP07_lm) #p = 0.076741
par(mfrow=c(1,1))
plot(AP_comb_07$`410390060_01` ~ AP_comb_07$`947c72aa269258cc_56971`, col=1, 
     pch=16, main  = "Amazon Park -- July 10-18, 2020", xlab = "sensor", 
     ylab = "monitor")
abline(AP07_lm)

# extract August week
AP_comb_08 <- AP_combine_data %>%
  monitor_subsetData(
    tlim = c(20200813, 20200821),
    timezone = timezone
  )

# run lm monitor~sensor
AP08_lm <- lm(AP_comb_08$`410390060_01` ~ AP_comb_08$`947c72aa269258cc_56971`)
summary(AP08_lm)
par(mfrow=c(2,2))
plot(AP08_lm)
shapiro.test(AP08_lm$residuals) #p-value = 0.0003979 not normally distributed 
ncvTest(AP08_lm) #p = 0.0014064
par(mfrow=c(1,1))
plot(AP_comb_08$`410390060_01` ~ AP_comb_08$`947c72aa269258cc_56971`, col=1, 
     pch=16, main  = "Amazon Park -- August 13-21, 2020", xlab = "sensor", 
     ylab = "monitor")
abline(AP08_lm)

# finish lms for other months 

##################### Oakridge 2 ##############################################
# create Oakridge monitor obj
Oa_monitor <- LRAPA_monitors2020 %>%
  monitor_subset(monitorIDs = "410392013_01")

# combine sensor and monitor
Oa2_combine <- list(Oa_sensor, Oa_monitor)
Oa2_combine <- monitor_combine(Oa2_combine)

# extract July week 
Oa2_combine_data <- Oa2_combine$data
Oa2_comb_07 <- Oa2_combine_data %>%
  monitor_subsetData(
    tlim = c(20200720, 20200728),
    timezone = timezone
  )

# run lm monitor~sensor
Oa207_lm <- lm(Oa2_comb_07$`410392013_01` ~ Oa2_comb_07$f2ace631a501333b_38681)
summary(Oa207_lm)
par(mfrow=c(2,2))
plot(Oa207_lm)
library(car)
shapiro.test(Oa207_lm$residuals) #p-value = 0.01791 not normally distributed 
ncvTest(Oa207_lm) #p = 0.076741
par(mfrow=c(1,1))
plot(Oa2_comb_07$`410392013_01` ~ Oa2_comb_07$f2ace631a501333b_38681, col=1, 
     pch=16, main  = "Oakridge 2 -- July 20-28, 2020", xlab = "sensor", 
     ylab = "monitor")
abline(Oa207_lm)

# finish lms for other months 
