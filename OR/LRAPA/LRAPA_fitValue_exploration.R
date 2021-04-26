# Author: Astrid Sanna
# Issue: fitValue_exploration #6 (MazamaScience/reports)
# Date: 4/21/2021

# ----- Libraries and Functions ------------------------------------------------
library(MazamaCoreUtils)
library(AirSensor)
library(PWFSLSmoke)
library(lubridate)

source("OR/LRAPA/sensorMonitorData.R")
source("OR/LRAPA/sensorMonitorFit.R")
source("OR/LRAPA/fitValueTimeSeries.R")

# ----- load LRAPA sensor and monitirs file from archiveDir --------------------
archiveDir <- "C:/Users/astri/Mirror/Mazamascience/Projects/Data/LRAPA"
LRAPA_sensors <- get(load(file.path(archiveDir, "LRAPA_pas.rda")))
LRAPA_monitors <- get(load(file.path(archiveDir, "LRAPA_monitors.rda")))

# ----- Amazon Park: load pat file ---------------------------------------------
# * setup -----
setArchiveBaseDir(archiveDir)
getArchiveBaseDir()
timezone = "America/Los_Angeles"
monitorID <- "410390060_01"

# * pat -----
Amazon_Park <- pat_load(
  id = "947c72aa269258cc_56971",
  startdate = 20200701,
  enddate = 20201101,
  timezone = timezone)

# ----- data exploration -------------------------------------------------------
# * first week of July -----
# check if under non-smoky conditions the time window affects the fit values
# 1 week window
fitValues07 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200708,
  modelParameters = c("pm25", "humidity"))

print(fitValues07)
#   r.squared intercept  pm25 humidity
# 1     0.978     3.765 0.446    -0.02

# 5 days window
fitValues07_5 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200706,
  modelParameters = c("pm25", "humidity"))

print(fitValues07_5)
#   r.squared intercept  pm25 humidity
# 1     0.979     3.787 0.452   -0.023

# 10  days window
fitValues07_10 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200701,
  enddate = 20200711,
  modelParameters = c("pm25", "humidity"))

print(fitValues07_10)
#   r.squared intercept  pm25 humidity
# 1     0.976     3.846 0.444    -0.02

# under non-smoky conditions the fit values seem to remain fairly constant


# * smoky week in Sep -----
fitValues09 <- sensorMonitorFit(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID ,
  startdate = 20200905,
  enddate = 20200912,
  modelParameters = c("pm25", "humidity"))

print(fitValues09)
#   r.squared intercept  pm25 humidity
# 1     0.933   -34.902 0.716    0.496

# ---- Fit Values Timeseries  --------------------------------------------------
# * w = 7 -----
FitValues_w7 <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 7 # days
)
View(FitValues_w7)

# Setup
names(FitValues_w7)
r.squared <- FitValues_w7$r.squared
intercept <- FitValues_w7$intercept
pm25 <- FitValues_w7$pm25
humidity <- FitValues_w7$humidity
se.monitorFit.mu <- FitValues_w7$se.monitorFit.mu
enddate <- FitValues_w7$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]
?seq

# Plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main="Fit Values -- July-Oct, 2020 -- 7 Days", xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=enddate, y=r.squared, col = colors()[640], lwd=2)
lines( x=enddate, y=pm25, col = colors()[641], lwd=2 )
lines( x=enddate, y=humidity, col = colors()[642], lwd=2 )
lines( x=enddate, se.monitorFit.mu, col = colors()[644], lty="dashed", lwd=2 )
?plot


# Legend:
labels <- c(
  "R squared (7 d)","Sensor PM25 (7 d)","Humidity (7 d)", "Monitor Fit SE Avg (7 d)"
)
legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid","dashed"),
  lwd=2,
  col= c(colors()[640], colors()[641], colors()[642], colors()[644])
)

# * w = 5 -----
FitValues_w5 <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 5 # days
)
View(FitValues_w5)

# Setup
names(FitValues_w5)
r.squared <- FitValues_w5$r.squared
intercept <- FitValues_w5$intercept
pm25 <- FitValues_w5$pm25
humidity <- FitValues_w5$humidity
se.monitorFit.mu <- FitValues_w5$se.monitorFit.mu
enddate <- FitValues_w5$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot w/ intercept
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main="Fit Values -- July-Oct, 2020 -- 5 Days", ylim = c(-2,2),
     xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=enddate, y=intercept, col = colors()[461], lwd=2)
lines( x=enddate, y=pm25, col = colors()[616], lwd=2 )
lines( x=enddate, y=humidity, col = colors()[122], lwd=2 )
lines( x=enddate, se.monitorFit.mu, col = colors()[131], lwd=2, lty="dashed" )
lines( x=enddate, y=r.squared, col = colors()[636], lwd=2)

## Legend:
labels <- c(
<<<<<<< HEAD
  "Intercept" ,"Sensor PM25 (5 d)","Humidity (5 d)", "Monitor Fit SE Avg (5 d)", 
  "R squared (5 d)"
)
=======
 "Intercept" ,"Sensor PM25 (5 d)","Humidity (5 d)", "Monitor Fit SE Avg (5 d)",
 "R squared (5 d)"
 )
>>>>>>> d7978be38acbfcc5ffb48326ef7bd4408720594d
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid","solid","dashed", "solid"),
  lwd=2,
  col= c(colors()[461],colors()[636], colors()[616], colors()[122], colors()[131])
)

# Plot w/ ONLY intercept
plot(enddate, intercept, xaxt="n", type="n",
     main="Fit Intercept -- July-Oct, 2020 -- 5 Days",
     xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=enddate, y=intercept, col = colors()[461], lwd=2)


# * w = 10 -----
FitValues_w10 <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 10 # days
)
View(FitValues_w10)

# Setup
names(FitValues_w10)
r.squared <- FitValues_w10$r.squared
intercept <- FitValues_w10$intercept
pm25 <- FitValues_w10$pm25
humidity <- FitValues_w10$humidity
se.monitorFit.mu <- FitValues_w10$se.monitorFit.mu
enddate <- FitValues_w10$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main="Fit Values -- July-Oct, 2020 -- 10 Days", xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=enddate, y=r.squared, col = colors()[498], lwd=2)
lines( x=enddate, y=pm25, col = colors()[53], lwd=2)
lines( x=enddate, y=humidity, col = colors()[52],lwd=2)
lines( x=enddate, se.monitorFit.mu, col = colors()[32], lty="dashed", lwd=2)


# Legend:
labels <- c(
  "R squared (10 d)","Sensor PM25 (10 d)","Humidity (10 d)", "Monitor Fit SE Avg (10 d)"
)
legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid","dashed"),
  lwd = 2,
  col= c(colors()[498], colors()[53], colors()[52], colors()[32])
)

# * Full plot -----
# setup
se.monitorFit.mu <- FitValues_w5$se.monitorFit.mu # highest se.monitorFit.mu -- best for x-axis range
a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main="Fit Values -- July-Oct, 2020 -- 5/7/10 Days", xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )

# 5 days
r.squared <- FitValues_w5$r.squared
intercept <- FitValues_w5$intercept
pm25 <- FitValues_w5$pm25
humidity <- FitValues_w5$humidity
se.monitorFit.mu <- FitValues_w5$se.monitorFit.mu
enddate <- FitValues_w5$enddate

lines( x=enddate, y=r.squared, col = colors()[636], lwd=2)
lines( x=enddate, y=pm25, col = colors()[616], lwd=2 )
lines( x=enddate, y=humidity, col = colors()[122], lwd=2 )
lines( x=enddate, se.monitorFit.mu, col = colors()[131], lwd=2, lty="dashed" )

# 7 days
r.squared <- FitValues_w7$r.squared
intercept <- FitValues_w7$intercept
pm25 <- FitValues_w7$pm25
humidity <- FitValues_w7$humidity
se.monitorFit.mu <- FitValues_w7$se.monitorFit.mu
enddate <- FitValues_w7$enddate

lines( x=enddate, y=r.squared, col = colors()[640], lwd=2)
lines( x=enddate, y=pm25, col = colors()[641], lwd=2 )
lines( x=enddate, y=humidity, col = colors()[642], lwd=2 )
lines( x=enddate, se.monitorFit.mu, col = colors()[644], lty="dashed", lwd=2 )

# 10 days
r.squared <- FitValues_w10$r.squared
intercept <- FitValues_w10$intercept
pm25 <- FitValues_w10$pm25
humidity <- FitValues_w10$humidity
se.monitorFit.mu <- FitValues_w10$se.monitorFit.mu
enddate <- FitValues_w10$enddate

# Plot
lines( x=enddate, y=r.squared, col = colors()[498], lwd=2)
lines( x=enddate, y=pm25, col = colors()[53], lwd=2)
lines( x=enddate, y=humidity, col = colors()[52],lwd=2)
lines( x=enddate, se.monitorFit.mu, col = colors()[32], lty="dashed", lwd=2)


# Legend
colors_5d <- c(colors()[636], colors()[616], colors()[122], colors()[131])
colors_7d <- c(colors()[640], colors()[641], colors()[642], colors()[644])
colors_10d <- c(colors()[498], colors()[53], colors()[52], colors()[32])

labels_5d <- c("R squared (5 d)","Sensor PM25 (5 d)","Humidity (5 d)", "Monitor Fit SE Avg (5 d)")
labels_7d <- c("R squared (7 d)","Sensor PM25 (7 d)","Humidity (7 d)", "Monitor Fit SE Avg (7 d)")
labels_10d <- c("R squared (10 d)","Sensor PM25 (10 d)","Humidity (10 d)", "Monitor Fit SE Avg (10 d)")

labels <- c(labels_5d, labels_7d, labels_10d)

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid","dashed",
        "solid", "solid","solid","dashed",
        "solid", "solid","solid","dashed"),
  lwd=2,
  col= c(colors_5d, colors_7d, colors_10d))

# * pm25 only -----
pm25 <- FitValues_w5$pm25 # highest pm25 -- best for x-axis range
enddate <- FitValues_w5$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]


# plot
plot(enddate, pm25, xaxt="n", type="n",
     main="PM2.5 Fit Values -- July-Oct, 2020 -- 5/7/10 Days", xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d") )

pm25 <- FitValues_w5$pm25
lines( x=enddate, y=pm25, col = colors()[616], lwd=2 )

pm25 <- FitValues_w7$pm25
lines( x=enddate, y=pm25, col = colors()[641], lwd=2 )

pm25 <- FitValues_w10$pm25
lines( x=enddate, y=pm25, col = colors()[53], lwd=2)

labels <- c("Sensor PM25 (5 d)", "Sensor PM25 (7 d)", "Sensor PM25 (10 d)")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * r.squared only -----
r.squared <- FitValues_w5$r.squared # highest r.squared -- best for x-axis range
enddate <- FitValues_w5$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]


# plot
plot(enddate, r.squared, xaxt="n", type="n",
     main="R squared Fit Values -- July-Oct, 2020 -- 5/7/10 Days", xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d") )

r.squared <- FitValues_w5$r.squared
lines( x=enddate, y=r.squared, col = colors()[616], lwd=2 )

r.squared <- FitValues_w7$r.squared
lines( x=enddate, y=r.squared, col = colors()[641], lwd=2 )

r.squared <- FitValues_w10$r.squared
lines( x=enddate, y=r.squared, col = colors()[53], lwd=2)

labels <- c("R squared (5 d)", "R squared (7 d)", "R squared (10 d)")

legend(
  "bottomright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * humidity only -----
humidity <- FitValues_w5$humidity # highest humidity -- best for x-axis range
enddate <- FitValues_w5$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]


# plot
plot(enddate, humidity, xaxt="n", type="n",
     main= "Humidity Fit Values -- July-Oct, 2020 -- 5/7/10 Days", xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

humidity <- FitValues_w5$humidity
lines( x=enddate, y=humidity, col = colors()[616], lwd=2 )

humidity <- FitValues_w7$humidity
lines( x=enddate, y=humidity, col = colors()[641], lwd=2 )

humidity <- FitValues_w10$humidity
lines( x=enddate, y=humidity, col = colors()[53], lwd=2)

labels <- c("Humidity (5 d)", "Humidity (7 d)", "Humidity (10 d)")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * se.monitorFit.mu only -----
se.monitorFit.mu<- FitValues_w5$se.monitorFit.mu # highest se.monitorFit.mu-- best for x-axis range
enddate <- FitValues_w5$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]


# plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit SE Avg -- July-Oct, 2020 -- 5/7/10 Days", xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

se.monitorFit.mu<- FitValues_w5$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[616], lwd=2 )

se.monitorFit.mu<- FitValues_w7$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[641], lwd=2 )

se.monitorFit.mu<- FitValues_w10$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[53], lwd=2)

labels <- c("Monitor Fit SE Avg (5 d)", "Monitor Fit SE Avg (7 d)", "Monitor Fit SE Avg (10 d)")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * SE of fitted monitor values: simple vs multiliear models (w = 7) -------------------

FitValues_p <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = "pm25",
  windowSize = 7 # days
)
View(FitValues_p)

FitValues_ph <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 7 # days
)
View(FitValues_ph)

FitValues_pht <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity", "temperature"),
  windowSize = 7 # days
)
View(FitValues_pht)

# Setup
se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit SE Avg -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

se.monitorFit.mu <- FitValues_p$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[616], lwd=2 )

se.monitorFit.mu <- FitValues_ph$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[641], lwd=2 )

se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[53], lwd=2)

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * Fitted monitor values: simple vs multiliear models (w = 7) -------------------------
# Setup
monitorFit.mu <- FitValues_pht$monitorFit.mu # highest se.monitorFit.mu-- best for x-axis range
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit Avg -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

monitorFit.mu <- FitValues_p$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[616], lwd=4, lty = "solid" )

monitorFit.mu <- FitValues_ph$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[641], lwd=3, lty = "dashed"  )

monitorFit.mu <- FitValues_pht$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[53], lwd=2, lty = "dotted" )

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "dashed","dotted"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# lines overlap "perfectly". Check fit values.

# df monitor fit values
monitorFit.mu.p <- FitValues_p$monitorFit.mu
monitorFit.mu.p <- as.data.frame(monitorFit.mu.p)
monitorFit.mu.p$monitorFit.mu.ph <- FitValues_ph$monitorFit.mu
monitorFit.mu.p$monitorFit.mu.pht <- FitValues_pht$monitorFit.mu
View(monitorFit.mu.p)

# * SE of fitted monitor values: simple vs multiliear models (w = 5) -------------------

FitValues_p <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = "pm25",
  windowSize = 5 # days
)

FitValues_ph <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 5 # days
)

FitValues_pht <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity", "temperature"),
  windowSize = 5 # days
)

# Setup
se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit SE Avg (w = 5) -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

se.monitorFit.mu <- FitValues_p$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[616], lwd=2 )

se.monitorFit.mu <- FitValues_ph$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[641], lwd=2 )

se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[53], lwd=2)

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * Fitted monitor values: simple vs multiliear models (w = 5) -------------------------
# Setup
monitorFit.mu <- FitValues_pht$monitorFit.mu # highest se.monitorFit.mu-- best for x-axis range
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit Avg (w = 5) -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

monitorFit.mu <- FitValues_p$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[616], lwd=4, lty = "solid" )

monitorFit.mu <- FitValues_ph$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[641], lwd=3, lty = "dashed"  )

monitorFit.mu <- FitValues_pht$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[53], lwd=2, lty = "dotted" )

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "dashed","dotted"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# lines overlap "perfectly". Check fit values.

# df monitor fit values
monitorFit.mu.p <- FitValues_p$monitorFit.mu
monitorFit.mu.p <- as.data.frame(monitorFit.mu.p)
monitorFit.mu.p$monitorFit.mu.ph <- FitValues_ph$monitorFit.mu
monitorFit.mu.p$monitorFit.mu.pht <- FitValues_pht$monitorFit.mu
View(monitorFit.mu.p)

# * SE of fitted monitor values: simple vs multiliear models (w = 10) -------------------

FitValues_p <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = "pm25",
  windowSize = 10 # days
)

FitValues_ph <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 10 # days
)

FitValues_pht <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity", "temperature"),
  windowSize = 10 # days
)

# Setup
se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit SE Avg (w = 10) -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

se.monitorFit.mu <- FitValues_p$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[616], lwd=2 )

se.monitorFit.mu <- FitValues_ph$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[641], lwd=2 )

se.monitorFit.mu <- FitValues_pht$se.monitorFit.mu
lines( x=enddate, y=se.monitorFit.mu, col = colors()[53], lwd=2)

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "solid","solid"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# * Fitted monitor values: simple vs multiliear models (w = 10) -------------------------
# Setup
monitorFit.mu <- FitValues_pht$monitorFit.mu # highest se.monitorFit.mu-- best for x-axis range
enddate <- FitValues_pht$enddate

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Plot
plot(enddate, monitorFit.mu, xaxt="n", type="n",
     main= "Monitor Fit Avg (w = 10) -- July-Oct, 2020 -- Simple- vs Multi-linear",
     xlab="Date", ylab = "Values")
axis( 1, at=date.at, format(date.at,"%b %d"))

monitorFit.mu <- FitValues_p$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[616], lwd=4, lty = "solid" )

monitorFit.mu <- FitValues_ph$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[641], lwd=3, lty = "dashed"  )

monitorFit.mu <- FitValues_pht$monitorFit.mu
lines( x=enddate, y=monitorFit.mu, col = colors()[53], lwd=2, lty = "dotted" )

labels <- c("PM2.5", "PM2.5+Humidity", "PM2.5+Humidity+Temperature")

legend(
  "topright",
  legend = labels,
  lty=c("solid", "dashed","dotted"),
  lwd=2,
  col= c(colors()[616], colors()[641], colors()[53]))

# lines overlap "perfectly". Check fit values.

# df monitor fit values
monitorFit.mu.p <- FitValues_p$monitorFit.mu
monitorFit.mu.p <- as.data.frame(monitorFit.mu.p)
monitorFit.mu.p$monitorFit.mu.ph <- FitValues_ph$monitorFit.mu
monitorFit.mu.p$monitorFit.mu.pht <- FitValues_pht$monitorFit.mu
View(monitorFit.mu.p)

# ---- Timeseries with EPA params/fit -----------------------------------

# * Fit Values Timeseries with EPA params -----
FitValues_w5 <- fitValueTimeSeries(
  pat = Amazon_Park,
  ws_monitor = LRAPA_monitors,
  monitorID = monitorID,
  startdate = 20200701,
  enddate = 20201031,
  modelParameters = c("pm25", "humidity"),
  windowSize = 5 # days
)
View(FitValues_w5)

# Create a df with EPA parameters of length(enddate). Values will be overlayed on
# timeseries later
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
# PA_cf1[(avgAB)] slope = PurpleAir higher correction factor data averaged
enddate <- FitValues_w5$enddate
PA_cf1_avgAB <- rep(0.534, times = length(enddate))
EPA_param <- as.data.frame(PA_cf1_avgAB)
# RH slope = relative humidity
EPA_param$RH <- rep(- 0.0844, times = length(enddate))
# EPA intercept
EPA_param$EPA_intercept <- rep(5.604, times = length(enddate))
View(EPA_param)

# Setup
names(FitValues_w5)
intercept <- FitValues_w5$intercept
pm25 <- FitValues_w5$pm25
humidity <- FitValues_w5$humidity
EPA_PA_cf1_avgAB <- EPA_param$PA_cf1_avgAB
EPA_RH <- EPA_param$RH
EPA_intercept <- EPA_param$EPA_intercept

a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-10-31T00:00:00') ),
  n = length(enddate)
)
x.date <- as_datetime(a$y)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,200,7)]

# Timeseries July-Oct w/ intercept + EPA params
plot(enddate, se.monitorFit.mu, xaxt="n", type="n",
     main="Fit Values + EPA param -- July-Oct, 2020 -- 5 Days", ylim = c(-2,6),
     xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=enddate, y=intercept, col = colors()[461], lwd=2)
lines( x=enddate, y=pm25, col = colors()[616], lwd=2 )
lines( x=enddate, y=humidity, col = colors()[122], lwd=2 )
lines( x=enddate, y=EPA_PA_cf1_avgAB, col = colors()[640], lwd=2)
lines( x=enddate, y=EPA_RH, col = colors()[641], lwd=2)
lines( x=enddate, y=EPA_intercept, col = colors()[642], lwd=2)

# Legend:
labels <- c(
  "Intercept" ,"Sensor PM25 (5 d)","Humidity (5 d)", "EPA PA_cf1(avgAB)", "EPA RH", "EPA Intercept"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid", "solid", "solid", "solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616], colors()[122],
         colors()[640], colors()[641], colors()[642])
)

# * One week (20200701 - 20200706) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_1 <-sensorMonitorData(pat = Amazon_Park,
                                        ws_monitor = LRAPA_monitors,
                                        monitorID = monitorID,
                                        startdate = 20200701,
                                        enddate = 20200706)
View(sensorMonitorData_1)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_1$pm25 - 0.0844*sensorMonitorData_1$humidity +5.604)
sensorMonitorData_1$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_1$EPA_PM25_corrected

# PA PM25 Fit
names(sensorMonitorData_1)
PA_lm <- lm(pm25_monitor ~ pm25 + humidity, data = sensorMonitorData_1)
summary(PA_lm)
sensorMonitorData_1$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_1)
sensorMonitorData_1$PA_pm25_fit <- round(sensorMonitorData_1$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_1$PA_pm25_fit

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-07-01T00:00:00'), ymd_hms('2020-07-06T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# timeseries axes
datetime <- sensorMonitorData_1$datetime
pm25_monitor <- sensorMonitorData_1$pm25_monitor

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n",
     main="FRM vs EPA PA corrected PM25 -- July 1-5, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_1$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "topleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)



# * One week (20200726 - 20200731) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_2 <-sensorMonitorData(pat = Amazon_Park,
                                        ws_monitor = LRAPA_monitors,
                                        monitorID = monitorID,
                                        startdate = 20200726,
                                        enddate = 20200731)
View(sensorMonitorData_2)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_2$pm25 - 0.0844*sensorMonitorData_2$humidity +5.604)
sensorMonitorData_2$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_2$EPA_PM25_corrected

# PA PM25 Fit
names(sensorMonitorData_2)
PA_lm <- lm(pm25_monitor ~ pm25 + humidity, data = sensorMonitorData_2)
summary(PA_lm)
sensorMonitorData_2$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_2)
sensorMonitorData_2$PA_pm25_fit <- round(sensorMonitorData_2$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_2$PA_pm25_fit

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-07-26T00:00:00'), ymd_hms('2020-07-31T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# timeseries axes
datetime <- sensorMonitorData_2$datetime
pm25_monitor <- sensorMonitorData_2$pm25_monitor

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n",
     main="FRM vs EPA PA corrected PM25 -- July 26-30, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_2$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "topleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)


# * One week (20200910 - 20200915) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_3 <-sensorMonitorData(pat = Amazon_Park,
                                        ws_monitor = LRAPA_monitors,
                                        monitorID = monitorID,
                                        startdate = 20200910,
                                        enddate = 20200915)
View(sensorMonitorData_3)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_3$pm25 - 0.0844*sensorMonitorData_3$humidity +5.604)
sensorMonitorData_3$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_3$EPA_PM25_corrected

# PA PM25 Fit
names(sensorMonitorData_3)
PA_lm <- lm(pm25_monitor ~ pm25 + humidity, data = sensorMonitorData_3)
summary(PA_lm)
sensorMonitorData_3$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_3)
sensorMonitorData_3$PA_pm25_fit <- round(sensorMonitorData_3$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_3$PA_pm25_fit

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-09-10T00:00:00'), ymd_hms('2020-09-15T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# timeseries axes
datetime <- sensorMonitorData_3$datetime
pm25_monitor <- sensorMonitorData_3$pm25_monitor

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n",
     main="FRM vs EPA PA corrected PM25 -- Sep 10-14, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_3$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)



# * One week (20201004 - 20201009) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_4 <-sensorMonitorData(pat = Amazon_Park,
                                        ws_monitor = LRAPA_monitors,
                                        monitorID = monitorID,
                                        startdate = 20201001,
                                        enddate = 20201006)
View(sensorMonitorData_4)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_4$pm25 - 0.0844*sensorMonitorData_4$humidity +5.604)
sensorMonitorData_4$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_4$EPA_PM25_corrected

# PA PM25 Fit
names(sensorMonitorData_4)
PA_lm <- lm(pm25_monitor ~ pm25 + humidity, data = sensorMonitorData_4)
summary(PA_lm)
sensorMonitorData_4$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_4)
sensorMonitorData_4$PA_pm25_fit <- round(sensorMonitorData_4$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_4$PA_pm25_fit

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-10-01T00:00:00'), ymd_hms('2020-10-06T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# timeseries axes
datetime <- sensorMonitorData_4$datetime
pm25_monitor <- sensorMonitorData_4$pm25_monitor

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n",
     main="FRM vs EPA PA corrected PM25 -- Oct 1-5, 2020",
     xlab="Date", ylab = "Values" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_4$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)










# ------------------------ Oakridge2: load pat file ----------------------------
Oakridge2 <- pat_load(
  id = "f2ace631a501333b_38681", 
  startdate = 20200701, 
  enddate = 20201101, 
  timezone = timezone)
print(Oakridge2$meta$pwfsl_closestDistance) # 13.14887 m 
monitorID_2 <- Oakridge2$meta$pwfsl_closestMonitorID # 410392013_01

Oa2_lm_ex <- pat_externalFit(Oakridge2, showPlot = TRUE)
# * One week (20200910 - 20200915) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_3_Oa2 <-sensorMonitorData(pat = Oakridge2,
                                            ws_monitor = LRAPA_monitors,
                                            monitorID = monitorID_2,
                                            startdate = 20200910,
                                            enddate = 20200915)
View(sensorMonitorData_3_Oa2)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_3_Oa2$pm25 - 0.0844*sensorMonitorData_3_Oa2$humidity +5.604)
sensorMonitorData_3_Oa2$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_3_Oa2$EPA_PM25_corrected

# PA PM25 Fit 
names(sensorMonitorData_3_Oa2)
PA_lm <- lm(pm25 ~ pm25_monitor + humidity, data = sensorMonitorData_3_Oa2)
summary(PA_lm)
sensorMonitorData_3_Oa2$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_3_Oa2)
sensorMonitorData_3_Oa2$PA_pm25_fit <- round(sensorMonitorData_3_Oa2$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_3_Oa2$PA_pm25_fit

# Timeseries axes
datetime <- sensorMonitorData_3_Oa2$datetime
pm25_monitor <- sensorMonitorData_3_Oa2$pm25_monitor

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-09-10T00:00:00'), ymd_hms('2020-09-15T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n", 
     main="FRM vs EPA PA corrected PM25 (Oakridge 2) -- Sep 10-14, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_3_Oa2$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)




# ------------------------ Oakridge3: load pat file ----------------------------
Oakridge3 <- pat_load(
  id = "799da69adac45e75_38631", 
  startdate = 20200701, 
  enddate = 20201101, 
  timezone = timezone)

print(Oakridge3$meta$pwfsl_closestDistance) # 2.571477 m 
monitorID_2 <- Oakridge3$meta$pwfsl_closestMonitorID # 410392013_01 (same as Oakridge2)

Oa3_lm_ex <- pat_externalFit(Oakridge3, showPlot = TRUE)
# * One week (20200910 - 20200915) timeseries including EPA linear fit -----
# Create df
sensorMonitorData_3_Oa3 <-sensorMonitorData(pat = Oakridge3,
                                            ws_monitor = LRAPA_monitors,
                                            monitorID = monitorID_2,
                                            startdate = 20200910,
                                            enddate = 20200915)
View(sensorMonitorData_3_Oa3)

# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*sensorMonitorData_3_Oa3$pm25 - 0.0844*sensorMonitorData_3_Oa3$humidity +5.604)
sensorMonitorData_3_Oa3$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- sensorMonitorData_3_Oa3$EPA_PM25_corrected

# PA PM25 Fit 
names(sensorMonitorData_3_Oa3)
PA_lm <- lm(pm25 ~ pm25_monitor + humidity, data = sensorMonitorData_3_Oa3)
summary(PA_lm)
sensorMonitorData_3_Oa3$PA_pm25_fit <- predict(PA_lm, newdata= sensorMonitorData_3_Oa3)
sensorMonitorData_3_Oa3$PA_pm25_fit <- round(sensorMonitorData_3_Oa3$PA_pm25_fit)
PA_pm25_fit <- sensorMonitorData_3_Oa3$PA_pm25_fit

# Timeseries axes
datetime <- sensorMonitorData_3_Oa3$datetime
pm25_monitor <- sensorMonitorData_3_Oa3$pm25_monitor

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-09-10T00:00:00'), ymd_hms('2020-09-15T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# timeseries axes
datetime <- sensorMonitorData_3_Oa3$datetime
pm25_monitor <- sensorMonitorData_3_Oa3$pm25_monitor

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n", 
     main="FRM vs EPA PA corrected PM25 (Oakridge 3) -- Sep 10-14, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=sensorMonitorData_3_Oa3$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)



# ------------------------ Springfield City Hall: load pat file ----------------
Springfield <- pat_load(
  id = "12426967f1fc742c_10606", 
  startdate = 20200701, 
  enddate = 20201101, 
  timezone = timezone)

print(Springfield$meta$pwfsl_closestDistance) # 7.657129 m 
monitorID_3 <- Springfield$meta$pwfsl_closestMonitorID # 410392013_01 (same as Oakridge2)

Springfield_lm_ex <- pat_externalFit(Springfield, showPlot = TRUE)

# * One week (20200910 - 20200915) timeseries including EPA linear fit -----
# Create df
Springfield <-sensorMonitorData(pat = Springfield,
                                ws_monitor = LRAPA_monitors,
                                monitorID = monitorID_3,
                                startdate = 20200910,
                                enddate = 20200915)
View(Springfield)
# Apply EPA equation using raw PA pm25
# EPA equation: PM2.5 corrected= 0.534*[PA_cf1(avgAB)] - 0.0844*RH +5.604
EPA_PM25_corrected <- (0.534*Springfield$pm25 - 0.0844*Springfield$humidity +5.604)
Springfield$EPA_PM25_corrected <- round(EPA_PM25_corrected)
EPA_PM25_corrected <- Springfield$EPA_PM25_corrected

# PA PM25 Fit 
names(Springfield)
PA_lm <- lm(pm25 ~ pm25_monitor + humidity, data = Springfield)
summary(PA_lm)
Springfield$PA_pm25_fit <- predict(PA_lm, newdata= Springfield)
Springfield$PA_pm25_fit <- round(Springfield$PA_pm25_fit)
PA_pm25_fit <- Springfield$PA_pm25_fit

# Timeseries axes
datetime <- Springfield$datetime
pm25_monitor <- Springfield$pm25_monitor

# Timeseries setup
a <- approx(
  c( ymd_hms('2020-09-10T00:00:00'), ymd_hms('2020-09-15T00:00:00') ),
  n = length(datetime)
)
x.date <- as_datetime(a$y)
length(x.date)

dr <- range(x.date)
date.at <- seq(dr[1], dr[2], by="day")[seq(1,120,1)]

# Plot w/ intercept
plot(datetime, pm25_monitor, xaxt="n", type="n", ylim = c(50, 600),
     main="FRM vs EPA PA corrected PM25 (Springfield City Hall) -- Sep 10-14, 2020",
     xlab="Date", ylab = "PM2.5" )
axis( 1, at=date.at, format(date.at,"%b %d") )
lines( x=datetime, y=pm25_monitor, col = colors()[461], lwd=2)
lines( x=datetime, y=EPA_PM25_corrected, col = colors()[616], lwd=2)
lines( x=datetime, y=Springfield$pm25, col = colors()[631], lwd=2)
lines( x=datetime, y=PA_pm25_fit, col = colors()[640], lwd=2)

## Legend:
labels <- c(
  "FRM measured PM25", "EPA PA corrected PM25", "PA measured PM25", "PA PM25 Fit"
)
legend(
  "bottomleft",
  legend = labels,
  lty=c("solid", "solid"),
  lwd=2,
  col= c(colors()[461], colors()[616],colors()[631], colors()[640])
)

