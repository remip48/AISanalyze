## Resubmission

This is a resubmission of version 2.0.1. The issues raised in the previous CRAN review have been addressed as follows:

* Removed the redundant "Tools for" from the package description.
* Added the package documentation URL to the `DESCRIPTION` file (references describing the package and its methods are currently in preparation).
* Replaced all uses of `T` and `F` with `TRUE` and `FALSE`.
* Removed `\dontrun{}` from examples.
* Removed console output using `cat()` where possible, and replaced with `message()` for remaining cases.
* Removed package installation commands from `inst/examples/AISanalyze_examples.R`.
* Re-ran `R CMD check --as-cran` successfully.

## R CMD check results

0 errors | 0 warnings | 0 notes
