#' Estimate vessel characteristics
#'
#' Estimates the most likely vessel characteristics for each `mmsi` from AIS
#' messages, including ship type, length, width, draught, IMO number, and
#' vessel name. Estimates are based on the most frequent values, giving greater
#' weight to records with more complete information.
#'
#' @param ais_data AIS data frame containing the columns `mmsi`, `shiptype`,
#'   `length`, `width`, `draught`, `imo`, and `name`. Another vessel identifier
#'   may be used if the column is named `mmsi`.
#' @param threshold_length Maximum valid vessel length (m). Larger values are
#'   set to `NA`.
#' @param threshold_draught Maximum valid draught (m). Larger values are set
#'   to `NA`.
#' @param threshold_width Maximum valid vessel width (m). Larger values are
#'   set to `NA`.
#' @param weight_complete_data Weight assigned to records containing both
#'   vessel length and ship type.
#'
#' @return A list containing:
#' \itemize{
#' \item `estimated_values`: Estimated vessel characteristics for each `mmsi`.
#' \item `summary`: Summary statistics for each `mmsi`, including:
#'   \itemize{
#'   \item Number of AIS positions.
#'   \item Number of non-missing values for `length`, `shiptype`, `width`,
#'     `draught`, `imo`, and `name`.
#'   \item All valid values observed for each characteristic.
#'   \item The most likely value for each characteristic.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#'
#' out <- AISinfos(ais_data = ais)}
#' @export

AISinfos <- function(ais_data,
                     threshold_length = 475,
                     threshold_draught = 30,
                     threshold_width = 75,
                     weight_complete_data = 10) {

  ais_data <- ais_data %>%
    dplyr::mutate(shiptype = ifelse(stringr::str_remove_all(shiptype, " ") == "", NA, shiptype),
                  length = as.numeric(length),
                  draught = as.numeric(draught),
                  width = as.numeric(width),
                  imo = as.numeric(imo),
                  shiptype = as.character(shiptype),
                  name = as.character(name),
                  length = ifelse(length == 0 | length > threshold_length, NA, length),
                  draught = ifelse(draught == 0 | draught > threshold_draught, NA, draught),
                  width = ifelse(width == 0 | width > threshold_width, NA, width)) %>%
    dplyr::group_by(mmsi, shiptype, length, width, draught, imo, name) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n,
                  n_weighted = ifelse(!is.na(length) & !is.na(shiptype) & !is.na(width) & !is.na(draught) & !is.na(imo) & !is.na(name),
                                      n*weight_complete_data,
                                      n)) %>%
    as.data.frame()

  all_infos_numeric <- purrr::map(c("length", "draught", "width", "imo"), function(c) {

    out <- ais_data %>%
      dplyr::group_by(mmsi) %>%
      dplyr::summarise(n_point_mmsi = sum(n),
                       n_point_no_NA_c = sum(n[!is.na(as.numeric(get(c)))]),
                       all_values_c = paste(sort(unique(stats::na.omit(as.numeric(get(c))))), collapse = ", "),
                       n_unique_values = length(unique(stats::na.omit(as.numeric(get(c))))),
                       Estimated_c = all_values_c,
                       .groups = "drop") %>%
      dplyr::ungroup()

    if (any(out$n_unique_values > 1)) {
      mmsi_to_correct <- out %>%
        dplyr::filter(n_unique_values > 1)

      unique_values <- ais_data %>%
        dplyr::filter(mmsi %in% unique(mmsi_to_correct$mmsi) & !is.na(get(c))) %>%
        dplyr::group_by(mmsi, get(c)) %>%
        dplyr::summarise(n_weighted = sum(n_weighted),
                         n = sum(n),
                         .groups = "drop") %>%
        dplyr::ungroup()

      colnames(unique_values)[2] <- "c"

      unique_values <- unique_values %>%
        dplyr::group_by(mmsi) %>%
        dplyr::summarise(Estimated_c = c[which.max(n_weighted)],
                         n_point_no_NA_c = n[which.max(n_weighted)],
                         .groups = "drop") %>%
        dplyr::ungroup()

      mmsi_to_correct <- mmsi_to_correct %>%
        dplyr::select(-c(Estimated_c, n_point_no_NA_c)) %>%
        dplyr::left_join(unique_values, by = "mmsi")

      out <- purrr::map_dfr(list(out %>%
                                   dplyr::filter(n_unique_values < 2),
                                 mmsi_to_correct), function(file) {
                                   file %>%
                                     dplyr::mutate(Estimated_c = as.numeric(Estimated_c),
                                                   n_point_no_NA_c = as.numeric(n_point_no_NA_c))
                                 })

    } else {
      out <- out %>%
        dplyr::mutate(Estimated_c = as.numeric(Estimated_c),
                      n_point_no_NA_c = as.numeric(n_point_no_NA_c))
    }

    colnames(out) <- stringr::str_replace_all(colnames(out),
                                              "c",
                                              c)

    return(out %>%
             dplyr::select(dplyr::all_of(stats::na.omit(c("mmsi",
                                                          ifelse(c == "length", "n_point_mmsi", NA),
                                                          paste0("n_point_no_NA_", c),
                                                          paste0("all_values_", c),
                                                          paste0("Estimated_", c))))))
  })

  all_infos_character <- purrr::map(c("shiptype", "name"), function(c) {

    ais_cleaned <- ais_data %>%
      dplyr::mutate(!!c := ifelse(tolower(stringr::str_remove_all(as.character(get(c)), " ")) %in% c("undefined", "unknown", "", "na"),
                                  NA,
                                  as.character(get(c))))

    out <- ais_cleaned %>%
      dplyr::group_by(mmsi) %>%
      dplyr::summarise(n_point_no_NA_c = sum(n[!is.na(get(c))]),
                       all_values_c = paste(sort(unique(stats::na.omit(get(c)))), collapse = ", "),
                       n_unique_values = length(unique(stats::na.omit(get(c)))),
                       Estimated_c = all_values_c,
                       .groups = "drop") %>%
      dplyr::ungroup()

    if (any(out$n_unique_values > 1)) {
      mmsi_to_correct <- out %>%
        dplyr::filter(n_unique_values > 1)

      unique_values <- ais_cleaned %>%
        dplyr::filter(mmsi %in% unique(mmsi_to_correct$mmsi) & !is.na(get(c))) %>%
        dplyr::group_by(mmsi, get(c)) %>%
        dplyr::summarise(n_weighted = sum(n_weighted),
                         n = sum(n),
                         .groups = "drop")

      colnames(unique_values)[2] <- "c"

      unique_values <- unique_values %>%
        dplyr::ungroup() %>%
        dplyr::group_by(mmsi) %>%
        dplyr::summarise(Estimated_c = c[which.max(n_weighted)],
                         n_point_no_NA_c = n[which.max(n_weighted)],
                         .groups = "drop") %>%
        dplyr::ungroup()

      mmsi_to_correct <- mmsi_to_correct %>%
        dplyr::select(-c(Estimated_c, n_point_no_NA_c)) %>%
        dplyr::left_join(unique_values, by = "mmsi")

      out <- purrr::map_dfr(list(out %>%
                                   dplyr::filter(n_unique_values < 2),
                                 mmsi_to_correct),
                            rbind)
    }

    colnames(out) <- stringr::str_replace_all(colnames(out),
                                              "c",
                                              c)

    return(out %>%
             dplyr::mutate(!!paste0("Estimated_", c) := ifelse(get(paste0("Estimated_", c)) == "", NA, get(paste0("Estimated_", c)))) %>%
             dplyr::select(dplyr::all_of(c("mmsi",
                                           paste0("n_point_no_NA_", c),
                                           paste0("all_values_", c),
                                           paste0("Estimated_", c)))))
  })

  ## to merge
  estimated_values <- all_infos_numeric[[1]]

  for (i in 2:length(all_infos_numeric)) {
    estimated_values <- estimated_values %>%
      dplyr::left_join(all_infos_numeric[[i]], by = "mmsi")
  }

  for (i in 1:length(all_infos_character)) {
    estimated_values <- estimated_values %>%
      dplyr::left_join(all_infos_character[[i]], by = "mmsi")
  }

  cat("\nWarnings are printed if any value of length, draught, width or imo can not be transformed to numeric (set as `NA`),
      or any value of shiptype and name can not be transformed to character (set as `NA`).\n")

  return(list(estimated_values = estimated_values %>%
                dplyr::select(mmsi,
                              dplyr::all_of(colnames(.)[stringr::str_detect(colnames(.), "Estimated")])),
              summary = estimated_values))

}
