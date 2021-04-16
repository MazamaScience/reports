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

  # ----- Return ----------------------------------------------------

  # TODO:  extract the fit parameters and r-squared and put them in a named vector
  
  # Ugly hack but it works
  fitValues <- c(summary(model)$adj.r.squared, model$coefficients, coef(summary(model))[, "Std. Error"])
  names(fitValues) <- c("r.squared", "intercept", names(model$coefficients)[-1], 
                        "se.intercept", "se.pm25", "se.humidity")
  fitDF <- as.data.frame(t(round(fitValues, 3)))
  
  return(fitDF)
}
  

  
    
 

