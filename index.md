# AISanalyze

## Documentation

A complete step-by-step workflow is available in the [User
guide](https://remip48.github.io/AISanalyze/articles/AISanalyze.html).

📖 Full documentation, tutorials and function reference:
<https://remip48.github.io/AISanalyze/>

## Overview

AISanalyze is an R package providing a fast and reproducible workflow
for preprocessing Automatic Identification System (AIS) vessel tracking
data for environmental and ecological research. It streamlines common
preprocessing tasks through a small set of user-friendly functions,
including vessel trajectory reconstruction, GPS correction,
interpolation, and the extraction of vessel positions around target
locations or time periods.

The package emphasizes computational efficiency and reproducibility,
allowing large AIS datasets to be prepared for downstream analyses in
seconds to minutes. Its main functionalities include:

- estimating vessel travel distance, time, and speed;
- correcting GPS errors and delays;
- identifying AIS base stations and aircraft;
- interpolating vessel positions;
- extracting vessel positions around target locations and times;
- retrieving vessel characteristics (ship type, length, width, draught,
  IMO number, and vessel name).

## Installation

``` r

# install.packages("remotes")
remotes::install_github("remip48/AISanalyze")
```

## Main functions

| Function | Description |
|----|----|
| [`AIStravel()`](https://remip48.github.io/AISanalyze/reference/AIStravel.md) | Estimate travelled distance, time and speed |
| [`AISidentify_stations_aircraft()`](https://remip48.github.io/AISanalyze/reference/AISidentify_stations_aircraft.md) | Identify AIS stations and aircraft |
| [`AIScorrect_speed()`](https://remip48.github.io/AISanalyze/reference/AIScorrect_speed.md) | Correct GPS errors and delays |
| [`AISinterpolate()`](https://remip48.github.io/AISanalyze/reference/AISinterpolate.md) | Interpolate vessel positions |
| [`AISextract()`](https://remip48.github.io/AISanalyze/reference/AISextract.md) | Extract vessels around target locations |
| [`AISinfos()`](https://remip48.github.io/AISanalyze/reference/AISinfos.md) | Estimate vessel characteristics |

## Example

``` r
library(AISanalyze)
data("ais")
data("point_to_extract")

## define the Unix time (seconds since 1970-01-01)
ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime)))
point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime)))

## Run the worklow:
results <- ais |>
  AIStravel() |>
  AISidentify_stations_aircraft() |>
  dplyr::filter(!station & !high_speed) |>
  AIScorrect_speed() |>
  AISinterpolate(.,
                 type_interpolation = "maximum_gap_seconds",
                 maximum_gap_seconds = 60) |>
  AISextract(data = point_to_extract, 
             search_into_radius_m = 10000)
```

## Performance

Total execution time for the complete workflow (for 100 target data
points and using 4 CPU cores):

| AIS dataset size | 100,000 points | 1,000,000 points | 2,500,000 points |
|------------------|----------------|------------------|------------------|
| Time             | 14 sec         | 68 sec           | 146 sec          |

## Citation

``` r

citation("AISanalyze")
```

If you use AISanalyze, please cite:

Pigeault R., Ruser A., Ramírez-Martínez N.C., Geelhoed S.C.V., Haelters
J., Nachtsheim D.A., Schaffeld T., Sveegaard S., Siebert U., Gilles A.
(2024). Maritime traffic alters distribution of the harbour porpoise in
the North Sea. *Marine Pollution Bulletin.* 208: 116925.

## License

This project is licensed under the MIT License. See the `LICENSE` file
for details.

## Contributing

Contributions are welcome! Whether you would like to report a bug,
suggest a new feature, or contribute code or documentation, please read
our
[CONTRIBUTING.md](https://remip48.github.io/AISanalyze/CONTRIBUTING.md)
guide to get started.

## Support

Please use the GitHub issue tracker to report bugs, request features, or
ask questions. For other enquiries, you may also contact the package
author directly.
