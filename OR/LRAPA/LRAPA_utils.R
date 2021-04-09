# Utility functions for the LRAPA co-location report


#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#'
#' @title Linear model fitting of PurpleAir and federal PWFSL time series data
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param ws_monitor PWFSLSmoke Timeseries \emph{ws_monitor} object.
#' @param monitorID Monitor ID specifying a single monitor when \empy{ws_monitor}
#' contains multiple time series.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param modelParameters One or more parameters from "pm25|humidity|temperature"
#' for a uni- or multi-variate model.
#'
#' @description Produces a linear model between data from PurpleAir and data
#' from the closest PWFSL monitor.
#'
#' Dates can be anything that is understood by \code{lubridate::ymd()}
#' including any of the following recommended formats:
#'
#' \itemize{
#' \item{\code{YYYYmmdd}}
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#'
#' @note Data used for the model fit will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. The exception being when
#' \code{enddate} is less than 24 hours after \code{startdate}. In that case, a
#' single day is returned.
#'
#' Dates will be interpreted as local time for the sensor-monitor pair.
#'
#' @note No QC is applied to either the sensor or the monitor data. Both should
#' be validated before attempting this analysis. \emph{(caveat emptor)}
#'
#' @return A linear model, fitting the hourly aggregated `pat` PurpleAir
#' readings to the provided PWFSL monitor readings.
#'
#' @examples
#' \donttest{
#' library(PWFSLSmoke)
#' library(AirSensor)
#'
#' archiveDir <- file.path("~/Data/LRAPA")
#' setArchiveBaseDir(archiveDir)
#'
#' # Get data from local archive
#' pas <- get(load(file.path(archiveDir, "LRAPA_pas.rda")))
#'
#' pat <- pat_loadMonth(
#'   id = "12426967f1fc742c_10606",
#'   pas = pas,
#'   datestamp = 202007,
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Closest monitor
#' monitorID <- "410391009_01"
#'
#' ws_monitor <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))
#'
#' model <-
#'   sensorMonitorFit(
#'     pat,
#'     ws_monitor,
#'     monitorID = monitorID,
#'     startdate = 20200701,
#'     enddate = 20200708,
#'     modelParameters = c("pm25")
#'   )
#'
#' coeffs <- model$coefficients
#' print(coeffs)
#'
#' # Now you can examine the model or make predictions with it.
#' }

sensorMonitorFit <- function(
  pat = NULL,
  ws_monitor = NULL,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL,
  modelParameters = c("pm25", "humidity")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(ws_monitor)
  MazamaCoreUtils::stopIfNull(modelParameters)
  
  validParameters <- c("pm25", "humidity", "temperature")
  unrecognizedParameters <- setdiff(modelParameters, validParameters)
  
  if ( length(unrecognizedParameters > 0) ) {
    stop(sprintf("modelParameter '%s' is not recognized", unrecognizedParameters))
  }
  
  # Single monitors don't need to specify monitorID
  if ( nrow(ws_monitor$meta) == 1 )
    monitorID <- ws_monitor$meta$monitorID
  
  # ----- Create dataframe of hourly data --------------------------------------
  
  # pat data
  pat <-
    pat %>%
    pat_filterDate(startdate, enddate, timezone = "America/Los_Angeles") %>%
    pat_aggregate() %>%
    pat_extractData() %>%
    dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
    dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity) %>%
    dplyr::mutate_all(round)
  
  # monitor data
  ws_monitor <-
    ws_monitor %>%
    monitor_subset(
      monitorIDs = monitorID,
      tlim = c(startdate, enddate),
      timezone = "America/Los_Angeles"
    ) %>%
    monitor_extractData() %>%
    dplyr::rename(pm25_monitor = !!monitorID)
  
  # combine
  df <- dplyr::left_join(pat, ws_monitor, by = "datetime")
  
  # ----- Linear fit -----------------------------------------------------------
  
  # See ?stats::formula
  model <- lm(
    as.formula(paste("pm25_monitor ~ ", paste(modelParameters, collapse = " + "))),
    data = df,
    na.action = na.exclude
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(model)
  
}
