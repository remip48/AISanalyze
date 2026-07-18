#' Avoid columns in data with locations/timestamp to extract to be named as mmsi
#'
#' @param data
#'
#' @return
#'
#' @examples
rename_columns_data <- function(data) {
  if (any(colnames(data) == "mmsi")) {
    colnames(data)[colnames(data) == "mmsi"] <- "initial_mmsi"
    cat("\nmmsi column in dataframe renamed as 'initial_mmsi'\n")
  }

  return(data)
}
