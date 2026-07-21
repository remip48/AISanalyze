# Calculate vessel travel metrics

Calculates the distance travelled (m), travel time (s), and speed (km/h)
between consecutive AIS positions for each vessel (`mmsi`).

## Usage

``` r
AIStravel(ais_data, crs_meters = 3035)
```

## Arguments

- ais_data:

  AIS data frame containing `timestamp`, `lon`, `lat`, and `mmsi`.
  `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
  and `lat` must be numeric. Another vessel identifier may be used if
  the column is named `mmsi`.

- crs_meters:

  CRS (in metres) used to calculate distances. Defaults to EPSG:3035.

## Value

The input AIS data with the following additional columns:

- `time_travelled`: Travel time (s) since the previous AIS position.

- `distance_travelled`: Distance travelled (m) since the previous AIS
  position.

- `speed_kmh`: Vessel speed (km/h).

- `X`, `Y`: coordinates in `crs_meters`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AISanalyze)
data("ais")

ais <- ais %>%
  mutate(timestamp = as.numeric(ymd_hms(datetime)))

out <- AIStravel(ais_data = ais)} # }
```
