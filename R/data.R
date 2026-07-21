#' Example AIS dataset
#'
#' A subset of Automatic Identification System (AIS) messages collected in the
#' North Sea on 1 November 2022. The dataset contains vessel positions together
#' with static and dynamic AIS information, including vessel identity,
#' navigational status, speed, heading, dimensions, draught and destination.
#'
#' The dataset is intended for demonstrating the main functions of
#' \pkg{AISanalyze}, including travel distance estimation, GPS error correction,
#' vessel characteristic estimation, interpolation and vessel extraction.
#' Original MMSI identifiers have been replaced with anonymized values to
#' protect vessel confidentiality.
#'
#' @format A data frame with AIS messages for multiple vessels. The main
#' variables include:
#' \describe{
#'   \item{datetime}{Date and time of the AIS message (UTC).}
#'   \item{mmsi}{Anonymized Maritime Mobile Service Identity (MMSI) of the vessel.}
#'   \item{lon}{Longitude (decimal degrees, WGS84).}
#'   \item{lat}{Latitude (decimal degrees, WGS84).}
#'   \item{navigational_status}{Reported navigational status.}
#'   \item{SOG}{Speed over ground (knots).}
#'   \item{Heading}{True heading (degrees).}
#'   \item{imo}{Reported International Maritime Organization (IMO) number.}
#'   \item{shiptype}{Reported vessel type.}
#'   \item{length}{Reported vessel length (m).}
#'   \item{name}{Reported vessel name.}
#'   \item{width}{Reported vessel width (m).}
#'   \item{draught}{Reported vessel draught (m).}
#' }
#'
#' @source Example subset extracted from AIS observations.
#'
#' @name ais
#' @docType data
NULL


#' Example extraction locations
#'
#' Example locations and timestamps used to demonstrate
#' [AISextract()] and interpolation at exact timestamps with
#' [AISinterpolate()]. Each row defines a target location and time for which
#' nearby vessel positions can be extracted or interpolated.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{point}{Unique identifier of the target location.}
#'   \item{lon, lat}{Longitude and latitude (WGS84).}
#'   \item{datetime}{Target date and time (UTC).}
#' }
#'
#' @seealso [AISextract()], [AISinterpolate()]
#'
#' @name point_to_extract
#' @docType data
NULL
