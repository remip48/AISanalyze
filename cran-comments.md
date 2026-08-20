## Resubmission

Resubmission of AISanalyze 3.1.1 following the previous CRAN pre-test.

The issues identified in the previous pre-test have been addressed:

* Revised the package description to remove the flagged spelling issue.
* Replaced the relative CONTRIBUTING.md link in the README with a full GitHub URL.
* Excluded cran-comments.md from the package build.
* Reduced the runtime of the AISextract() and AISinterpolate() examples to below the 5-second threshold.

R CMD check --as-cran completes with 0 errors, 0 warnings, and 0 notes.

The package was also checked using win-builder with R-devel on Windows, with 0 errors and 0 warnings. The only remaining NOTE is the expected “New submission” NOTE.

## Resubmission

This is a resubmission of version 3.1.1. The issues raised in the previous CRAN review have been addressed as follows:

* Removed the redundant "Tools for" from the package description.
* Added the package documentation URL to the `DESCRIPTION` file (a reference describing the package and its methods are currently in preparation).
* Replaced all uses of `T` and `F` with `TRUE` and `FALSE`.
* Removed `\dontrun{}` from examples.
* Removed console output using `cat()` where possible, and replaced with `message()` for remaining cases.
* Removed package installation commands from `inst/examples/AISanalyze_examples.R`.
* Re-ran `R CMD check --as-cran` successfully.

## R CMD check results

0 errors | 0 warnings | 0 note

