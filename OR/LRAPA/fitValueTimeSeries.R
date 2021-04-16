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

  # TODO

  # ----- Create date sequence ------------------------------------------------

  startdates <-
    MazamaCoreUtils::dateSequence(
      startdate = startdate,
      enddate = enddate,
      timezone = pat$meta$timezone
    )

  # ----- Loop over startdates -------------------------------------------------

  fitValuesList <- list()
  for ( startdate in startdates ) {

    enddate <- startdate + lubridate::ddays(windowSize)

    # TODO:  calculate linear fit by running sensorMonitorFitValues()

    # TODO:  save the start date and fit values in the list

  }

  # TODO:  convert fitValuesList into a datafame

  # ----- Return ---------------------------------------------------------------

  return(df)

}
