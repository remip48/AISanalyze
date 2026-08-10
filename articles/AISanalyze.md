# AISanalyze: User guide

## Introduction

AISanalyze provides a workflow to analyse Automatic Identification
System (AIS) data, including:

- estimating vessel travel distance, time and speed;
- correcting GPS errors and delays;
- identifying AIS stations and aircraft;
- interpolating vessel positions;
- extracting vessels around target locations;
- estimating vessel characteristics.

This vignette illustrates a typical workflow.

## Example data

``` r

data("ais")
data("point_to_extract")
```

Convert timestamps to Unix time.

``` r

ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))
point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime))
```

## Estimate travelled distance and speed

``` r

ais <- AIStravel(ais_data = ais)
```

Three variables are added:

- `distance_travelled`
- `time_travelled`
- `speed_kmh`

## Identify stations and aircraft

``` r

ais <- AISidentify_stations_aircraft(ais_data = ais)
```

Two logical variables are added:

- `station`
- `high_speed`

## Correct GPS errors

``` r

ais <- AIScorrect_speed(ais_data = ais)
```

This step corrects unrealistic speeds caused by GPS errors or
transmission delays.

## Interpolate vessel positions

The example below interpolates vessel positions every 60 seconds.

``` r

ais_interpolated_60sec <- AISinterpolate(
  ais_data = ais,
  type_interpolation = "maximum_time_interval",
  maximum_gap_seconds = 60
)
```

Alternatively, interpolation can be performed at exact timestamps.
Target locations and a search radius (m) can be specified to limit
interpolation to the area of interest and reduce computation time. The
`datetime` column can then be updated from the new `timestamp`.

``` r

ais_interpolated_exact_timestamps <- AISinterpolate(
  ais_data = ais,
  type_interpolation = "exact_timestamp",
  exact_timestamp = list(
    timestamp_to_interpolate = point_to_extract$timestamp,
    locations_of_interest = point_to_extract[c("lon", "lat")],
    radius = 200000
  )
)
```

The `datetime` column can be updated from the interpolated timestamps:

``` r

ais_interpolated_60sec$datetime <- lubridate::as_datetime(ais_interpolated_60sec$timestamp)

ais_interpolated_exact_timestamps$datetime <- lubridate::as_datetime(ais_interpolated_exact_timestamps$timestamp)
```

## Extract nearby vessels

Extract all vessel positions within 50 km and ±5 minutes of the target
locations and timestamps (`point_to_extract`).

``` r

AISextract(
  ais_data = ais_interpolated_60sec,
  data = point_to_extract,
  return_all_vessel_locations = TRUE,
  search_into_radius_m = 50000,
  interval_time_before = 300,
  interval_time_after = 300
)
```

Set `return_all_vessel_locations = FALSE` to return only the vessel
position at the target timestamps:

``` r

AISextract(
  ais_data = ais_interpolated_exact_timestamps,
  data = point_to_extract,
  return_all_vessel_locations = FALSE,
  search_into_radius_m = 50000,
  interval_time_before = 300,
  interval_time_after = 300
)
```

Alternatively, you can pass the centroids of a square grid to `data` and
set `search_shape = "square"` to extract vessel positions within square
grid cells:

``` r

AISextract(
  ais_data = ais_interpolated_exact_timestamps,
  data = point_to_extract,
  return_all_vessel_locations = FALSE, # or TRUE
  search_into_radius_m = 50000,
  search_shape = "square",
  interval_time_before = 300,
  interval_time_after = 300
)
```

## Estimate vessel characteristics

``` r

infos <- AISinfos(ais)

summary_values <- infos$summary
estimated_values <- infos$estimated_values
```

This function estimates the most likely vessel characteristics for each
MMSI, including ship type, dimensions, draught, IMO number, and name.
`summary_values` summarises all values found in the AIS data, whereas
`estimated_values` contains the estimated characteristic for each
vessel.

## Workflow summary

The recommended workflow is:

``` text
AIS data
│
▼
AIStravel()
│
▼
AISidentify_stations_aircraft() (optional)
│
▼
AIScorrect_speed() (optional)
│
▼
AISinterpolate()   (optional)
│
▼
AISextract()
│
▼
AISinfos()         (optional)
```
