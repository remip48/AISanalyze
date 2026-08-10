# Correct GPS errors in AIS tracks

Detects and corrects GPS errors and delayed AIS messages that generate
unrealistic vessel speeds, travelled distances, and travel times. Note:
For consecutive GPS errors, only the first point is removed to avoid
overcorrection.

## Usage

``` r
AIScorrect_speed(
  ais_data,
  crs_meters = 3035,
  threshold_speed_to_correct = 100,
  threshold_speed_to_correct_expr = function(speed_kmh) {
     return(15 +
    stats::median(speed_kmh[speed_kmh > 1], na.rm = TRUE) + 5 *
    stats::sd(speed_kmh[speed_kmh > 1 & speed_kmh < stats::quantile(speed_kmh[speed_kmh >
    1], 0.75)]))
 },
  nb_cores = 1,
  outfile = tempfile()
)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
  and `lat` must be numeric. Another vessel identifier may be used if
  the column is named `mmsi`.

- crs_meters:

  CRS (metres) used to calculate travelled distances. Defaults to
  EPSG:3035.

- threshold_speed_to_correct:

  Speed threshold (km/h) above which observations are corrected.

- threshold_speed_to_correct_expr:

  Function returning a vessel-specific speed threshold from `speed_kmh`.

- nb_cores:

  Number of CPU cores used.

- outfile:

  File used to save logs.

## Value

The input data with corrected travel metrics and the following columns:

- `speed_kmh_corrected`: Whether the speed was corrected.

- `time_travelled`: Travel time (s).

- `distance_travelled`: Travelled distance (m).

- `speed_kmh`: Vessel speed (km/h).

## Examples

``` r
library(AISanalyze)
data("ais")

# Define the Unix time (seconds since 1970-01-01)
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))

# calculate the travelled distance, time, and speed:
ais <- AIStravel(ais_data = ais)

# Correct speed:
out <- AIScorrect_speed(ais_data = ais,
                 crs_meters = 3035,
                 threshold_speed_to_correct = 100)
```
