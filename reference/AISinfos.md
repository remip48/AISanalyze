# Estimate vessel characteristics

Estimates the most likely vessel characteristics for each `mmsi` from
AIS messages, including ship type, length, width, draught, IMO number,
and vessel name. Estimates are based on the most frequent values, giving
greater weight to records with more complete information.

## Usage

``` r
AISinfos(
  ais_data,
  threshold_length = 475,
  threshold_draught = 30,
  threshold_width = 75,
  weight_complete_data = 10
)
```

## Arguments

- ais_data:

  AIS data frame containing the columns `mmsi`, `shiptype`, `length`,
  `width`, `draught`, `imo`, and `name`. Another vessel identifier may
  be used if the column is named `mmsi`.

- threshold_length:

  Maximum valid vessel length (m). Larger values are set to `NA`.

- threshold_draught:

  Maximum valid draught (m). Larger values are set to `NA`.

- threshold_width:

  Maximum valid vessel width (m). Larger values are set to `NA`.

- weight_complete_data:

  Weight assigned to records containing both vessel length and ship
  type.

## Value

A list containing:

- `estimated_values`: Estimated vessel characteristics for each `mmsi`.

- `summary`: Summary statistics for each `mmsi`, including:

  - Number of AIS positions.

  - Number of non-missing values for `length`, `shiptype`, `width`,
    `draught`, `imo`, and `name`.

  - All valid values observed for each characteristic.

  - The most likely value for each characteristic.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AISanalyze)
data("ais")

out <- AISinfos(ais_data = ais)} # }
```
