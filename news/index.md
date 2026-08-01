# Changelog

## AISanalyze 3.0.3 (2026-08-01)

### Bug fixes

- Correct a bug preventing helper function to be used in the parallel
  processing loop of AIScorrect_speed().

## AISanalyze 3.0.2 (2026-08-01)

### Bug fixes

- Load the helper function add_coordinates_meters() in parallel
  processing loop of AIScorrect_speed().

## AISanalyze 3.0.1 (2026-08-01)

### New features

- Added parallel processing to AISinterpolate() for the option
  type_interpolation = “maximum_gap_seconds”.
- Added parallel processing to AISextract().
- Added parallel processing to AIScorrect_speed().

### Improvements

- Redesigned the AISinfos() function to provide simplify the readability
  of the script and output results.
- Checked all functions performed well and fast with the abovementionned
  changes.

## AISanalyze 2.0.2 (2026-07-30)

### Improvements

- Simplify use and code of AISinterpolate().
- cite CONTRIBUTION.md in README

## AISanalyze 2.0.1 (2026-07-30)

### Improvements

- Finalized documentation and package metadata for the first CRAN
  release.
- Minor improvements to the package website and documentation.

## AISanalyze 2.0.0 (2026-07-20)

### New features

- Added comprehensive unit tests.
- Added a package vignette with a complete workflow.
- Added a package help page providing an overview of the public API.
- Added a pkgdown website with online documentation.

### Improvements

- Refactored large functions into smaller helper functions for improved
  maintainability.
- Simplified the package workflow.
- Improved and expanded function documentation and examples.
- Added continuous integration with GitHub Actions.
- Added `NEWS.md` and `CONTRIBUTING.md`.

### Bug fixes

- Fixed vessel extraction when only a single AIS position is available.
- Improved handling of edge cases throughout the package.

## AISanalyze 1.1.0 (2026-02-10)

### New features

- First public release on GitHub.
- Added the accompanying manuscript.

### Improvements

- Improved function documentation.

### Bug fixes

- Fixed
  [`AIScorrect_speed()`](https://remip48.github.io/AISanalyze/reference/AIScorrect_speed.md)
  for stationary vessels.
- Corrected `time_travelled` and `distance_travelled` for consecutive
  interpolated positions.

## AISanalyze 1.0.0 (2025-06-17)

### New features

- Initial release of the package.
