# Interpolate AIS positions

Interpolates vessel positions either: (depending on
`type_interpolation`)

- to ensure time intervals do not exceed a specified maximum
  (`= maximum_gap_seconds`).

- at user-defined timestamps (`= exact_timestamp`). Interpolation can
  optionally be restricted to specific regions to reduce computation
  time with `locations_of_interest` and `radius` arguments.

## Usage

``` r
AISinterpolate(
  ais_data,
  type_interpolation,
  maximum_gap_seconds,
  exact_timestamp = list(timestamp_to_interpolate, locations_of_interest, radius),
  crs_meters = 3035,
  nb_cores = 1,
  outfile = tempfile()
)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
  and `lat` must be numeric.

- type_interpolation:

  Interpolation mode: `"maximum_gap_seconds"` or `"exact_timestamp"`.

- maximum_gap_seconds:

  used when `type_interpolation = "maximum_gap_seconds"`: threshold
  above which AIS signals are interpolated.

- exact_timestamp:

  List used when `type_interpolation = "exact_timestamp"`, containing:

  - `timestamp_to_interpolate`

  - `locations_of_interest`: (optional) data frame with `lon` and `lat`
    columns corresponding to each `timestamp_to_interpolate`

  - `radius`: (optional) a search radius (m) around target locations

- crs_meters:

  CRS (in metres) used for distance calculations. Defaults to EPSG:3035.

- nb_cores:

  Number of CPU cores used.

- outfile:

  File used to save logs.

## Value

The interpolated AIS data with an additional column:

- `interpolated`: Whether the position was interpolated.

## Examples

``` r
data("ais")
data("point_to_extract")

# use only a sample for the example:
ais <- ais[20000:30000, ]

# Define the Unix time (seconds since 1970-01-01)
point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime))
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))

# calculate the travelled distance, time, and speed:
ais <- AIStravel(ais_data = ais)

# Interpolate all AIS signals further than > 120 seconds:
out <- AISinterpolate(ais_data = ais,
                      type_interpolation = "maximum_gap_seconds",
                      maximum_gap_seconds = 120, ## Alternatively, you can
                      ## interpolate at target timestamps with:
                      # exact_timestamp = list(
                      #      timestamp_to_interpolate = point_to_extract$timestamp,
                      #      locations_of_interest = data.frame(lon = point_to_extract$lon,
                      #                                         lat = point_to_extract$lat),
                      #      radius = 200000),
                      crs_meters = 3035)
```
