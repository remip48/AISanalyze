# Correct GPS errors in AIS tracks

Detects and corrects GPS errors and delayed AIS messages that generate
unrealistic vessel speeds, travelled distances, and travel times.

## Usage

``` r
AIScorrect_speed(
  ais_data,
  crs_meters = 3035,
  threshold_speed_to_correct = 100,
  threshold_speed_to_correct_expr = function(speed_kmh) {
     return(15 +
    stats::median(speed_kmh[speed_kmh > 1], na.rm = T) + 5 *
    stats::sd(speed_kmh[speed_kmh > 1 & speed_kmh < stats::quantile(speed_kmh[speed_kmh >
    1], 0.75)]))
 }
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

## Value

The input data with corrected travel metrics and the following columns:

- `speed_kmh_corrected`: Whether the speed was corrected.

- `time_travelled`: Travel time (s).

- `distance_travelled`: Travelled distance (m).

- `speed_kmh`: Vessel speed (km/h).

## Examples

``` r
if (FALSE) { # \dontrun{
library(AISanalyze)
data("ais")

ais <- ais %>%
  mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
  AIStravel(ais_data = .)

out <- AIScorrect_speed(ais_data = ais,
                 crs_meters = 3035,
                 threshold_speed_to_correct = 100)} # }
```
