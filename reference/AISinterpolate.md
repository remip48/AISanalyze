# Interpolate AIS positions

Interpolates vessel positions either: (depending on
`type_interpolation`)

- to ensure time intervals do not exceed a specified maximum
  (`maximum_time_interval`).

- at user-defined timestamps (`exact_timestamp`). Interpolation can
  optionally be restricted to a given radius within target locations to
  reduce computation time.

## Usage

``` r
AISinterpolate(
  ais_data,
  type_interpolation,
  maximum_time_interval = list(maximum_gap_seconds = 10 * 60),
  exact_timestamp = list(timestamp_to_interpolate = NULL, locations_of_interest = NULL,
    radius = 2e+05),
  crs_meters = 3035,
  nb_cores = 1,
  outfile = "log.txt"
)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
  and `lat` must be numeric.

- type_interpolation:

  Interpolation mode: `"maximum_time_interval"` or `"exact_timestamp"`.

- maximum_time_interval:

  List used when `type_interpolation = "maximum_time_interval"`,
  containing `maximum_gap_seconds`.

- exact_timestamp:

  List used when `type_interpolation = "exact_timestamp"`, containing:

  - `timestamp_to_interpolate`: Target timestamps.

  - `locations_of_interest`: Optional data frame with `lon` and `lat`
    columns corresponding to each `timestamp_to_interpolate`.

  - `radius`: Optional search radius (m) around each target location.

- crs_meters:

  CRS (in metres) used for distance calculations. Defaults to EPSG:3035.

- nb_cores:

  Number of CPU cores used if `type_interpolation = exact_timestamp`.

- outfile:

  File used to save logs when `type_interpolation = exact_timestamp`.

## Value

The interpolated AIS data with an additional column:

- `interpolated`: Whether the position was interpolated.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AISanalyze)
data("ais")
data("point_to_extract")

point_to_extract <- point_to_extract %>%
  mutate(timestamp = as.numeric(ymd_hm(datetime)))

ais <- ais %>%
  mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
  AIStravel(ais_data = .)

# to interpolate all vessel locations separated by > 60 seconds
out <- AISinterpolate(ais_data = ais,
               type_interpolation = "maximum_time_interval",
               maximum_time_interval = list(maximum_gap_seconds = 60),
               crs_meters = 3035)

# to interpolate all vessel locations at exact timestamps,
# within a radius of 200 000 meters around
# target locations
out <- AISinterpolate(ais_data = ais,
           type_interpolation = "exact_timestamp",
           exact_timestamp = list(
               timestamp_to_interpolate = point_to_extract$timestamp,
               locations_of_interest = data.frame(lon = point_to_extract$lon,
                                                 lat = point_to_extract$lat),
               radius = 200000),
           crs_meters = 3035)
           } # }
```
