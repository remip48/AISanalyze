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
  threshold_speed_to_correct_function = NULL,
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

- threshold_speed_to_correct_function:

  a function to estimate vessel-specific speed thresholds. The function
  can use all columns of ais_data as argument (such as `speed_kmh`,
  `distance_travelled` or `time_travelled`). If set to `NULL`, an
  internal function is used (see `details`).

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

## Details

When `threshold_speed_to_correct_function` is set to `NULL`, a
vessel-specific speed threshold is estimated from the observed vessel
`speed_kmh`. The threshold is calculated as:

\$\$ T = 15 + \operatorname{median}(v\_{\>1}) + 5 \times
\operatorname{SD}(v\_{\>1,\\v\<Q\_{0.75}}) \$\$

where \\v\_{\>1}\\ represents vessel speeds greater than 1 km/h, and
\\v\_{\>1,\\v\<Q\_{0.75}}\\ represents vessel speeds between 1 km/h and
the 75th percentile. The equation is designed to capture the typical
travelling speed of the vessel while accounting for variation in its
observed travelling speeds.

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
