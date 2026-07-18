#' Rename columns of AIS data to avoid mismatch with data (excepted MMSI and timestamp)
#'
#' @param ais_data AIS data
#' @param data data with timestamp and locations where to extract
#'
#' @return
#'
#' @examples
#'
rename_colums_ais <- function(ais_data, data) {
  colnam <- colnames(ais_data)
  if (any(colnam[!(colnam %in% c("timestamp", "mmsi"))] %in%
          c(colnames(data)))) {
    colnames(ais_data)[colnam %in% c(colnames(data)) & !(colnam %in%
                                                           c("timestamp", "mmsi"))] <- paste0("ais_", colnam[colnam %in%
                                                                                                               c(colnames(data)) & !(colnam %in% c("timestamp",
                                                                                                                                                   "mmsi"))])
    cat("\n", paste0("'", paste(colnam[colnam %in% colnames(data) &
                                         !(colnam %in% c("timestamp", "mmsi"))], collapse = ", "),
                     "'"), "columns in AIS data renamed as", paste0("'",
                                                                    paste(colnames(ais_data)[colnam %in% colnames(data) &
                                                                                               !(colnam %in% c("timestamp", "mmsi"))], collapse = ", "),
                                                                    "'"), "\n")
  }

  return(ais_data)
}
