
# AISanalyze

<!-- badges: start -->

[![R-CMD-check](https://github.com/remip48/AISanalyze/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/remip48/AISanalyze/actions/workflows/R-CMD-check.yaml)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21474293.svg)](https://doi.org/10.5281/zenodo.21474293)

[![Documentation](https://img.shields.io/badge/Documentation-pkgdown-blue)](https://remip48.github.io/AISanalyze/)

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/remip48/AISanalyze/blob/master/LICENSE.md)

<!-- badges: end -->

## Documentation

A complete step-by-step workflow is available in the [User
guide](https://remip48.github.io/AISanalyze/articles/AISanalyze.html).

📖 Full documentation, tutorials and function reference:
<https://remip48.github.io/AISanalyze>

## Overview

AISanalyze is an R package for analysing Automatic Identification System
(AIS) data. It provides tools to

-   estimate vessel travel distance, time and speed;
-   correct GPS errors and delays;
-   identify AIS base stations and aircraft;
-   interpolate vessel positions;
-   extract vessel positions around target locations and times;
-   estimate vessel characteristics (ship type, length, width, draught,
    IMO and name).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("remip48/AISanalyze")
```

## Main functions

| Function                          | Description                                 |
|-----------------------------------|---------------------------------------------|
| `AIStravel()`                     | Estimate travelled distance, time and speed |
| `AISidentify_stations_aircraft()` | Identify AIS stations and aircraft          |
| `AIScorrect_speed()`              | Correct GPS errors and delays               |
| `AISinterpolate()`                | Interpolate vessel positions                |
| `AISextract()`                    | Extract vessels around target locations     |
| `AISinfos()`                      | Estimate vessel characteristics             |

## Example

``` r
library(AISanalyze)

data("ais")
data("point_to_extract")

## define the Unix time (seconds since 1970-01-01)
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime)))
point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime)))

## process, correct, interpolate and extract AIS data:
ais <- AIStravel(ais)

ais_identified <- AISidentify_stations_aircraft(ais)

ais_corrected <- AIScorrect_speed(ais_identified)

ais_interpolated <- AISinterpolate(ais_data = ais_corrected,
                                   type_interpolation = "maximum_time_interval",
                                   maximum_time_interval = list(maximum_gap_seconds = 60))

ais_extracted <- AISextract(ais_data = ais_interpolated,
                            data = point_to_extract,
                            return_all_vessel_locations = T,
                            search_into_radius_m = 50000)

head(ais_extracted)
```

## Citation

If you use AISanalyze, please cite:

Pigeault R., Ruser A., Ramírez-Martínez N.C., Geelhoed S.C.V., Haelters
J., Nachtsheim D.A., Schaffeld T., Sveegaard S., Siebert U., Gilles A.
(2024). Maritime traffic alters distribution of the harbour porpoise in
the North Sea. *Marine Pollution Bulletin.* 208: 116925.

## License

This project is licensed under the MIT License. See the `LICENSE` file
for details.
