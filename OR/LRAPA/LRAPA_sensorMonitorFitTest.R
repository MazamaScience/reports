# test sensorMonitorFit() 

fitValues <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = "410390060_01",
  startdate = 20200701,
  enddate = 20200708,
  modelParameters = c("pm25", "humidity"))
#Error: object of type 'closure' is not subsettable
