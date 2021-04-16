fitValueTimeSeries <- function(
  pat = NULL,
  ws_monitor = NULL,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL,
  modelParameters = c("pm25", "humidity"),
  windowSize = 7 # days
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(ws_monitor)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(modelParameters)

  validParameters <- c("pm25", "humidity", "temperature")
  unrecognizedParameters <- setdiff(modelParameters, validParameters)
  if ( length(unrecognizedParameters) > 0 ) {
    stop(paste0(
      "The following parameters are not recognized: \"",
      paste(unrecognizedParameters, collapse = ", "),
      "\".  Please choose among [pm25|humidity|temperature]."
    ))
  }

  # ----- Create date sequence ------------------------------------------------

  startdates <-
    MazamaCoreUtils::dateSequence(
      startdate = startdate,
      enddate = enddate,
      timezone = pat$meta$timezone
    )

  # ----- Loop over startdates -------------------------------------------------

  fitValuesList <- list()
  for ( i in seq_along(startdates) ) {

    start <- startdates[i]
    end <- start + lubridate::ddays(windowSize)
    startstamp <- strftime(startdates[i], format = "%Y%m%d", tz = pat$meta$timezone)

    fitValues <- sensorMonitorFit(
      pat = pat,
      ws_monitor = ws_monitor,
      monitorID = "410390060_01",
      startdate = start,
      enddate = end,
      modelParameters = modelParameters
    )

    fitValuesList[[startstamp]] <- fitValues

  }

  df <- dplyr::bind_rows(fitValuesList)

  # ----- Return ---------------------------------------------------------------

  return(df)

}
