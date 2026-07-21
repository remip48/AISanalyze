# Extract AIS positions around target locations and times

Returns either (depending on `return_all_vessel_locations`):

- each vessel position at the target timestamps.

- or all vessel positions within a specified time window.

## Usage

``` r
AISextract(
  data,
  ais_data,
  crs_meters = 3035,
  return_all_vessel_locations = T,
  search_into_radius_m = 50000,
  interval_time_before = 5 * 60,
  interval_time_after = 5 * 60
)
```

## Arguments

- data:

  Data frame containing `timestamp`, `lon`, and `lat`. `timestamp` must
  be Unix time (seconds since 1970-01-01), while `lon` and `lat` must be
  numeric.

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp`, `lon`, and `lat` must be numeric. Another vessel
  identifier may be used if the column is named `mmsi`.

- crs_meters:

  CRS (metres) used to calculate distances. Defaults to EPSG:3035.

- return_all_vessel_locations:

  Logical. If `TRUE`, returns all vessel positions within the specified
  time window. Otherwise, returns only the closest position in time.

- search_into_radius_m:

  Search radius (m).

- interval_time_before:

  Time window (s) before each `data$timestamp`.

- interval_time_after:

  Time window (s) after each `data$timestamp`.

## Value

`data` joined with matching AIS positions. Rows are duplicated when
several vessel positions match a target location and time. If no vessel
is found, AIS columns (including `mmsi`) are filled with `NA`. The
output also includes `distance_vessel_to_location_m`, the distance (m)
between the target location and each vessel position.

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
  AIStravel(ais_data = .) %>%
  AISinterpolate(ais_data = .,
           type_interpolation = "exact_timestamp",
           exact_timestamp = list(
             timestamp_to_interpolate = point_to_extract$timestamp,
             locations_of_interest = data.frame(lon = point_to_extract$lon,
                                                lat = point_to_extract$lat),
             radius = 200000),
           crs_meters = 3035)

# to return all vessel positions around the target location/timestamps:
out <- AISextract(data = point_to_extract,
           ais_data = ais,
           crs_meters = 3035,
           return_all_vessel_locations = TRUE,
           search_into_radius_m = 50000,
           interval_time_before = 5 * 60,
           interval_time_after = 5 * 60)

# to return the position of each vessel closest in time to the target
# timestamps (around the target location)
out <- AISextract(data = point_to_extract,
           ais_data = ais,
           crs_meters = 3035,
           return_all_vessel_locations = FALSE,
           search_into_radius_m = 50000,
           interval_time_before = 5 * 60,
           interval_time_after = 5 * 60)
           } # }
```
