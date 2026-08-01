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
  search_shape = "circle",
  interval_time_before = 5 * 60,
  interval_time_after = 5 * 60,
  nb_cores = 1,
  outfile = "log.txt"
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
