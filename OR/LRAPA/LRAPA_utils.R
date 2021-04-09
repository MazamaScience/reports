# Utility functions for the LRAPA co-location report


#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#'
#' @title Create a data frame with PurpleAir and federal PWFSL time series data
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param ws_monitor PWFSLSmoke Timeseries \emph{ws_monitor} object.
#' @param monitorID Monitor ID specifying a single monitor when \empy{ws_monitor}
#' contains multiple time series.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#'
#' @description Produces a data frame appropriate for linear modeling with lm().
#' Data from PurpleAir is combined with data from the closest PWFSL monitor.
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
#' @note Data will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. The exception being when
#' \code{enddate} is less than 24 hours after \code{startdate}. In that case, a
#' single day is returned.
#'
#' Dates will be interpreted as local time for the sensor-monitor pair.
#'
#' @note No QC is applied to either the sensor or the monitor data. Both should
#' be validated before attempting any analysis. \emph{(caveat emptor)}
#'
#' @return A data frame with sensor and monitor hourly data.
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
#' df <-
#'   sensorMonitorFit(
#'     pat,
#'     ws_monitor,
#'     monitorID = monitorID,
#'     startdate = 20200701,
#'     enddate = 20200708
#'   )
#'
#' dplyr::glimpse(df) 
#'
#' }

sensorMonitorFit <- function(
  pat = NULL,
  ws_monitor = NULL,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(ws_monitor)

  # Single monitors don't need to specify monitorID
  if ( nrow(ws_monitor$meta) == 1 )
    monitorID <- ws_monitor$meta$monitorID

  # Use the single timezone from pat because ws_monitor might have more than one
  timezone <- pat$meta$timezone

  # ----- Create dataframe of hourly data --------------------------------------

  # pat data
  pat_data <-
    pat %>%
    pat_filterDate(startdate, enddate, timezone = timezone) %>%
    pat_aggregate() %>%
    pat_extractData() %>%
    dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
    dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity) %>%
    dplyr::mutate_all(round)

  # monitor data
  ws_monitor_data <-
    ws_monitor %>%
    monitor_subset(
      monitorIDs = monitorID,
      tlim = c(startdate, enddate),
      timezone = timezone
    ) %>%
    monitor_extractData() %>%
    dplyr::rename(pm25_monitor = !!monitorID)

  # combine
  df <- dplyr::left_join(pat_data, ws_monitor_data, by = "datetime")

  # ----- Return ---------------------------------------------------------------

  return(df)

}
