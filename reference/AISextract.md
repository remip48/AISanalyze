# Extract AIS positions around target locations and times

Returns either (depending on `return_all_vessel_locations`):

- the position of each vessel at the time closest to the target
  timestamps.

- or all vessel positions within a specified time window.

## Usage

``` r
AISextract(
  ais_data,
  data,
  crs_meters = 3035,
  return_all_vessel_locations = TRUE,
  search_into_radius_m = 50000,
  search_shape = "circle",
  interval_time_before = 5 * 60,
  interval_time_after = 5 * 60,
  nb_cores = 1,
  outfile = tempfile()
)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp`, `lon`, and `lat` must be numeric. Another vessel
  identifier may be used if the column is named `mmsi`.

- data:

  Data frame containing `timestamp`, `lon`, and `lat`. `timestamp` must
  be Unix time (seconds since 1970-01-01), while `lon` and `lat` must be
  numeric.

- crs_meters:

  CRS (metres) used to calculate distances. Defaults to EPSG:3035.

- return_all_vessel_locations:

  Logical. If `TRUE`, returns all vessel positions within the specified
  time window. Otherwise, returns only the closest position in time.

- search_into_radius_m:

  Search radius (m).

- search_shape:

  `"circle"` (default; selects vessels within `search_into_radius_m` of
  the target location) or `"square"` (selects vessels within
  `search_into_radius_m` in both the X and Y directions, useful for
  grid-based analyses).

- interval_time_before:

  Time window (s) before each `data$timestamp`.

- interval_time_after:

  Time window (s) after each `data$timestamp`.

- nb_cores:

  Number of CPU cores used.

- outfile:

  File used to save logs.

## Value

`data` joined with matching AIS positions. Rows are duplicated when
several vessel positions match a target location and time. If no vessel
is found, AIS columns (including `mmsi`) are filled with `NA`. The
output also includes `distance_vessel_to_location_m`, the distance (m)
between the target location and vessel positions.

## Examples

``` r
library(AISanalyze)
data("ais")
data("point_to_extract")

# Define the Unix time (seconds since 1970-01-01)
point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime))
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))

# calculate the travelled distance, time, speed, and interpolate AIS data:
ais <- ais |>
  AIStravel() |>
  AISinterpolate(type_interpolation = "exact_timestamp",
                 exact_timestamp = list(
             timestamp_to_interpolate = point_to_extract$timestamp,
             locations_of_interest = data.frame(lon = point_to_extract$lon,
                                                lat = point_to_extract$lat),
             radius = 200000),
                 crs_meters = 3035)

# Extract all vessel positions within the target time interval and radius:
out <- AISextract(ais_data = ais,
           data = point_to_extract,
           crs_meters = 3035,
           return_all_vessel_locations = TRUE,
           search_into_radius_m = 50000,
           interval_time_before = 5 * 60,
           interval_time_after = 5 * 60)
#> 
#> The columns 'datetime, lon, lat' in the AIS data have been renamed to 'ais_datetime, ais_lon, ais_lat'

# Extract each vessel position at the time closest to the target timestamps
# within the radius:
out <- AISextract(ais_data = ais,
           data = point_to_extract,
           crs_meters = 3035,
           return_all_vessel_locations = FALSE,
           search_into_radius_m = 50000,
           interval_time_before = 5 * 60,
           interval_time_after = 5 * 60)
#> 
#> The columns 'datetime, lon, lat' in the AIS data have been renamed to 'ais_datetime, ais_lon, ais_lat'
```
