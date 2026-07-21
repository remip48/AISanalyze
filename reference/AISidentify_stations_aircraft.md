# Identify AIS base stations and high-speed craft

Identify AIS base stations and high-speed craft

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
if (FALSE) { # \dontrun{
library(AISanalyze)
data("ais")

ais <- ais %>%
  mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
  AIStravel(.)

out <- AISidentify_stations_aircraft(ais_data = ais)} # }
```
