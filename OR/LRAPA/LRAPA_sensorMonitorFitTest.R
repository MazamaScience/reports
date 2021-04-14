# test sensorMonitorFit() 

fitValues <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200905,
  enddate = 20200913,
  modelParameters = c("pm25"))

View(fitValues)
