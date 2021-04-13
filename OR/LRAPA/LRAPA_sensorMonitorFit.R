sensorMonitorFit <- function(
  pat = NULL,
  ws_monitor = NULL,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL,
  modelParameters = c("pm25", "humidity")
) {

  # ----- Validate parameters ------------------------------------------------

  # TODO

  # ----- Create model ------------------------------------------------------

  df <- sensorMonitorData(
    pat = pat,
    ws_monitor = ws_monitor,
    monitorID = monitorID,
    startdate = startdate,
    enddate = enddate
  )

  # ----- Linear fit -----------------------------------------------------------

  # See ?stats::formula
  model <- lm(
    as.formula(paste("pm25_monitor ~ ", paste(modelParameters, collapse = " + "))),
    data = df,
    na.action = na.exclude
  )

  # ----- Return ---------------------------------------------------------------

  # TODO:  extract the fit parameters and r-squared and put them in a named vector

  return(fitValues)

}
