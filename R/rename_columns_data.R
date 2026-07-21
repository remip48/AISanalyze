#' Check column names of data
#'
#' Check if any column in `data` are called `mmsi`, and rename it if so to avoid conflict with AIS data.
#'
#' @param data data with target timestamps and locations to extract
#'
#' @return the same dataset, with columns that are renamed if conflict were detected
#' @keywords internal
#' @noRd
#'
rename_columns_data <- function(data) {
  if (any(colnames(data) == "mmsi")) {
    colnames(data)[colnames(data) == "mmsi"] <- "data_mmsi"
    cat("\nmmsi column in dataframe renamed as 'data_mmsi'\n")
  }

  return(data)
}
