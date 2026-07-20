#' AISanalyze
#'
#' Tools for analysing, cleaning, interpolating and extracting Automatic
#' Identification System (AIS) vessel data.
#'
#' ## Main functions:
#'
#' **Travel metrics**
#' * [AIStravel()]
#' * [AIScorrect_speed()]
#'
#' **Vessel characteristics**
#' * [AISidentify_stations_aircraft()]
#' * [AISinfos()]
#'
#' **Interpolation**
#' * [AISinterpolate()]
#'
#' **Extraction**
#' * [AISextract()]
#'
#' @docType package
#' @name AISanalyze
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
"_PACKAGE"
