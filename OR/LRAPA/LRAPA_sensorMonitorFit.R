################################################################
# Author: Astrid Sanna
# Issue: LRAPA colocated sensors #5 (MazamaScience/reports)
# Date: 4/13/2021
################################################################

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
  pm25 <- round(df$pm25)
  humidity <- round(df$humidity)

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
  fitValues <- data.frame(
    round(summary(model)$adj.r.squared,3),
    round(summary(model)$coefficients[1],3),
    round(summary(model)$coefficients[2],3),
    round(summary(model)$coefficients[3],3))
  
  fitValues <- fitValues %>%
    rename(
      R_sq = paste(names(fitValues[1])),
      intercept = paste(names(fitValues[2])),
      PM25 = paste(names(fitValues[3])),
      Humidity = paste(names(fitValues[4])))

  return(fitValues)

}
