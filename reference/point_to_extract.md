# Example extraction locations

Example locations and timestamps used to demonstrate
[`AISextract()`](https://remip48.github.io/AISanalyze/reference/AISextract.md)
and interpolation at exact timestamps with
[`AISinterpolate()`](https://remip48.github.io/AISanalyze/reference/AISinterpolate.md).
Each row defines a target location and time for which nearby vessel
positions can be extracted or interpolated.

## Format

A data frame with the following variables:

- point:

  Unique identifier of the target location.

- lon, lat:

  Longitude and latitude (WGS84).

- datetime:

  Target date and time (UTC).

## See also

[`AISextract()`](https://remip48.github.io/AISanalyze/reference/AISextract.md),
[`AISinterpolate()`](https://remip48.github.io/AISanalyze/reference/AISinterpolate.md)
