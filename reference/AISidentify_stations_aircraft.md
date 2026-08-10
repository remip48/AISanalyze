# Identify AIS base stations and high-speed craft

Stations and aircraft are identified from speed, distance and time only.
Other criteria (e.g. MMSIs with fewer than 9 digits) are not considered.

## Usage

``` r
AISidentify_stations_aircraft(ais_data, crs_meters = 3035)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
  and `lat` must be numeric. Another vessel identifier may be used if
  the column is named `mmsi`.

- crs_meters:

  CRS (metres) used to calculate distances. Defaults to EPSG:3035.

## Value

The input AIS data with the following additional columns:

- `station`: Whether the MMSI is classified as a base station.

- `high_speed`: Whether the MMSI is classified as a high-speed craft.

- `n_point_mmsi_initial_data`: Number of valid AIS positions for the
  MMSI.

- `id_mmsi_point_initial`: Sequential identifier of each AIS position.

## Examples

``` r
library(AISanalyze)
data("ais")

# Define the Unix time (seconds since 1970-01-01)
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))

# calculate the travelled distance, time, and speed:
ais <- AIStravel(ais_data = ais)

# Identify stations and aircrafts:
out <- AISidentify_stations_aircraft(ais_data = ais)
```
