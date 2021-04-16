# Issue: sensorMonitorFitValue #5 (MazamaScience/reports)
# Date: 4/13/2021


sensorMonitorFit <- function(
  pat = NULL,
  ws_monitor = NULL,
  monitorID = NULL,
  startdate = NULL,
  enddate = NULL,
  modelParameters = c("pm25", "humidity")
) {

  # ----- Validate parameters ------------------------------------------------

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

  # ----- Return ----------------------------------------------------

  # Ugly hack but it works
  fitValues <- c(summary(model)$adj.r.squared, model$coefficients, coef(summary(model))[, "Std. Error"])
  names(fitValues) <- c("r.squared", "intercept", names(model$coefficients)[-1], 
                        "se.intercept", "se.pm25", "se.humidity")
  fitDF <- as.data.frame(t(round(fitValues, 3)))
  fitDF$enddate <- enddate

  return(fitDF)

}






