# Reports

_Last updated January 31, 2021_

## Background

In 2021, Mazama Science is primarily focused on environmental time series data and maintains
a suite of R packages focused on air quality data management, processing,
analysis and visualization.

## Individual reports

Subdirectories in this repository will be broken up by US state with individual
reports available as `.md` or `.Rmd` files. These reports are written for 
internal use only, often by interns, and Mazama Science accepts no responsibility 
for the accuracy of the content.

Our decision to open this repository reflects our commitment to
modeling the transparency we hope to inspire in others working with 
publicly available environmental data.

* [California](./CA/README.md)

## R packages

Our suite of existing and planned R packages includes:

* **[MazamaCoreUtils](https://mazamascience.github.io/MazamaCoreUtils/)** -- reusable core functionality
* **[MazamaSpatialUtils](https://mazamascience.github.io/MazamaSpatialUtils/)** -- spatial metadata enhancement
* **[MazamaLocationUtils](https://mazamascience.github.io/MazamaLocationUtils/)** -- unique location IDs
* **MazamaTimeSeries** -- reusable functionality (filter, join, combine, _.etc_) for the `meta` + `data` data model
* **[PWFSLSmoke](https://mazamascience.github.io/PWFSLSmoke/)** (future **AirMonitor**) -- currently has data ingest, QC, analysis and base-R low level plotting for hourly monitoring data
* **AirMonitor** -- Air Quality specific analysis and specialty plots built on top of **MazamaTimeSeries**
* **AirMonitorIngest** -- per-provider functionality for ingesting hourly monitoring data from AirNow, AIRSIS, WRCC and others
* **[AirMonitorPlots](https://mazamascience.github.io/AirMonitorPlots/)** -- ggplot-based higher level plotting for hourly data
* **AirMonitorProducts** -- functions for creating Excel tables and other non-ggplot2 items
* **AirMonitorReports** -- a collection of Rmarkdown documents and associated helper functions for creating automatic reports
* **[AirSensor](https://mazamascience.github.io/AirSensor/)** -- data ingest, QC, analysis and diagnostic plotting for inexpensive sensor data (Purple Air)
* **PurpleAirAPI** -- R wrapper functions for the new PurpleAir API

