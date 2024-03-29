% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02b_AISidentify_stations_aircraft.R
\name{AISidentify_stations_aircraft}
\alias{AISidentify_stations_aircraft}
\title{Identify base-stations and high-speed craft (aircraft) among AIS data}
\usage{
AISidentify_stations_aircraft(
  ais_data,
  quantile_station = 0.975,
  threshold_distance_station = 1,
  quantile_high_speed = 0.97,
  threshold_high_speed = 110
)
}
\arguments{
\item{ais_data}{AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.}

\item{quantile_station}{Quantile (0 to 1) of distance, by mmsi, which is compared to threshold_distance_station to assess if the MMSI is a station or not: if below threshold_distance_station, MMSI is considered as stationary and is a station. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.}

\item{threshold_distance_station}{Threshold of distance (meters) used to assess if the MMSI is a station.}

\item{quantile_high_speed}{Quantile (0 to 1) of speed, by mmsi, which is compared to threshold_high_speed to assess if the MMSI is a aircraft or not: if above threshold_high_speed, MMSI is considered as a station. We used 0.97 to prevent misinterpretations from GPS errors.}

\item{threshold_high_speed}{Threshold of speed (km/h) used to assess if the MMSI is an aircraft.}
}
\value{
the AIS data with the columns:
\itemize{
\item station: if TRUE, the MMSI has been identified as a station.
\item high_speed: if TRUE, the MMSI has been identified as an high speed craft (specially used for aircraft).
\item any_NA_speed_kmh: if TRUE, at least one of the speeds of this MMSI has a speed as NA (so distance_travelled or time_travelled has a issue and the AIS data must be checked). Should not occur.
\item n_point_mmsi_initial_data: number of point of the MMSI in the AIS data after have removed the points with inexisting longitude and latitude.
\item id_mmsi_point_initial: identifier for the MMSI point after ordering, correcting and cleaning.}
}
\description{
Identify base-stations and high-speed craft (aircraft) among AIS data
}
\examples{
\dontrun{
data("ais")

library(dplyr)
library(lubridate)
ais <- ais \%>\%
  mutate(timestamp = as.numeric(ymd_hms(datetime)))
AISidentify_stations_aircraft(ais_data = ais)}
}
