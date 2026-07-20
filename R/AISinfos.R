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

  weight_complete_data <- as.numeric(weight_complete_data)

  temp <- ais_data %>%
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
    dplyr::mutate(real_n = n,
                  n = ifelse(!is.na(length) & !is.na(shiptype)
                             & !is.na(width) & !is.na(draught)
                             & !is.na(imo) & !is.na(name),
                             n*weight_complete_data,
                             n)) %>%
    as.data.frame() %>%
    dplyr::mutate(name = name)

  ## for length
  all_infos_numeric <- purrr::map(c("length", "draught", "width", "imo"), function(c) {

    out <- temp %>%
      dplyr::group_by(mmsi) %>%
      dplyr::summarise(npoint = sum(real_n),
                       npoint_all_XXX = sum(real_n[!is.na(as.numeric(get(c)))]),
                       npoint_all_XXX_weighted = sum(n[!is.na(as.numeric(get(c)))]),
                       all_XXX = paste(sort(unique(stats::na.omit(as.numeric(get(c))))), collapse = ", "),
                       n_XXX = length(unique(stats::na.omit(as.numeric(get(c))))),
                       XXX = all_XXX,
                       npoint_XXX = npoint_all_XXX,
                       npoint_XXX_weighted = npoint_all_XXX_weighted,
                       .groups = "drop") %>%
      dplyr::ungroup()

    if (any(out$n_XXX > 1)) {

      to_corr <- out %>%
        dplyr::filter(n_XXX > 1)

      uni_XXX <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp %>% dplyr::pull(get(c))), ] %>%
        dplyr::group_by(mmsi, get(c)) %>%
        dplyr::summarise(n_l = sum(n),
                         n_li = sum(real_n), .groups = "drop"
        ) %>%
        dplyr::ungroup()

      colnames(uni_XXX)[2] <- "XXX"

      uni_XXX <- uni_XXX %>%
        dplyr::group_by(mmsi) %>%
        dplyr::summarise(XXX = XXX[which.max(n_l)],
                         npoint_XXX = n_li[which.max(n_l)],
                         npoint_XXX_weighted = max(n_l), .groups = "drop") %>%
        dplyr::ungroup()

      to_corr <- to_corr %>%
        dplyr::select(-c(XXX, npoint_XXX, npoint_XXX_weighted)) %>%
        dplyr::left_join(uni_XXX, by = "mmsi") %>%
        dplyr::mutate(XXX = as.numeric(XXX),
               npoint_XXX = as.numeric(npoint_XXX),
               npoint_XXX_weighted = as.numeric(npoint_XXX_weighted))

      out <- purrr::map_dfr(list(out %>%
                            dplyr::filter(n_XXX < 2) %>%
                            dplyr::mutate(XXX = as.numeric(XXX),
                                   npoint_XXX = as.numeric(npoint_XXX),
                                   npoint_XXX_weighted = as.numeric(npoint_XXX_weighted)),
                          to_corr), function(f) {return(f)})

    } else {
      out <- out %>%
        dplyr::mutate(XXX = as.numeric(XXX),
               npoint_XXX = as.numeric(npoint_XXX),
               npoint_XXX_weighted = as.numeric(npoint_XXX_weighted))
    }

    colnames(out) <- stringr::str_replace_all(colnames(out),
                                     "XXX",
                                     c)

    return(out %>%
             dplyr::select(dplyr::all_of(c("mmsi",
                                    "npoint",
                                    paste0("npoint_", c),
                                    paste0("all_", c),
                                    c))))
  })

  all_infos_character <- purrr::map(c("shiptype", "name"), function(c) {

    out_XXX <- temp %>%
      dplyr::mutate(XXX = ifelse(tolower(stringr::str_remove_all(get(c), " ")) %in% c("undefined", "unknown", "", "na"), NA, get(c))) %>%
      dplyr::group_by(mmsi) %>%
      dplyr::summarise(npoint_all_XXX = sum(real_n[!is.na(get(c))]),
                       npoint_all_XXX_weighted = sum(n[!is.na(get(c))]),
                       all_XXX = paste(sort(unique(stats::na.omit(get(c)))), collapse = ", "),
                       n_XXX = length(unique(stats::na.omit(get(c)))),
                       XXX = all_XXX,
                       npoint_XXX = npoint_all_XXX,
                       npoint_XXX_weighted = npoint_all_XXX_weighted, .groups = "drop") %>%
      dplyr::ungroup()

    if (any(out_XXX$n_XXX > 1)) {

      to_corr <- out_XXX %>%
        dplyr::filter(n_XXX > 1)

      uni_XXX <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp %>% dplyr::pull(get(c))), ] %>%
        dplyr::mutate(XXX = ifelse(tolower(stringr::str_remove_all(get(c), " ")) %in% c("undefined", "unknown", "", "na"), NA, get(c))) %>%
        dplyr::filter(!is.na(get(c))) %>%
        dplyr::group_by(mmsi, get(c)) %>%
        dplyr::summarise(n_t = sum(n),
                         n_ti = sum(real_n), .groups = "drop"
        )

      colnames(uni_XXX)[2] <- "XXX"

      uni_XXX <- uni_XXX %>%
        dplyr::ungroup() %>%
        dplyr::group_by(mmsi) %>%
        dplyr::summarise(XXX = XXX[which.max(n_t)],
                         npoint_XXX = n_ti[which.max(n_t)],
                         npoint_XXX_weighted = max(n_t), .groups = "drop") %>%
        dplyr::ungroup()

      to_corr <- to_corr %>%
        dplyr::select(-c(XXX, npoint_XXX, npoint_XXX_weighted)) %>%
        dplyr::left_join(uni_XXX, by = "mmsi")

      out_XXX <- purrr::map_dfr(list(out_XXX %>%
                                 dplyr::filter(n_XXX < 2),
                               to_corr), function(f) {return(f)})

    }

    colnames(out_XXX) <- stringr::str_replace_all(colnames(out_XXX),
                                     "XXX",
                                     c)

    return(out_XXX %>%
             dplyr::select(dplyr::all_of(c("mmsi",
                                    paste0("npoint_", c),
                                    paste0("all_", c),
                                    c))))
  })

  ## to merge
  out <- all_infos_numeric[[1]] %>%
    dplyr::left_join(all_infos_numeric[[2]] %>%
                dplyr::select(-npoint), by = "mmsi") %>%
    dplyr::left_join(all_infos_numeric[[3]] %>%
                dplyr::select(-npoint), by = "mmsi") %>%
    dplyr::left_join(all_infos_numeric[[4]] %>%
                dplyr::select(-npoint), by = "mmsi") %>%
    dplyr::left_join(all_infos_character[[1]], by = "mmsi") %>%
    dplyr::left_join(all_infos_character[[2]], by = "mmsi") %>%
    dplyr::mutate(shiptype = ifelse(shiptype == "", NA, shiptype),
           name = ifelse(name == "", NA, name)) %>%
    dplyr::select(-dplyr::all_of(colnames(.)[(stringr::str_detect(colnames(.),
                                                 "npoint_") &
                                                stringr::str_detect(colnames(.),
                                                   "_weighted")) |
                                               stringr::str_detect(colnames(.),
                                                   "npoint_all_") |
                                               stringr::str_detect(colnames(.),
                                                   "n_")])) %>%
    dplyr::rename(n_point_mmsi = npoint)

  colnames(out)[stringr::str_detect(colnames(out), "npoint_")] <- paste0(stringr::str_replace_all(colnames(out)[stringr::str_detect(colnames(out), "npoint_")],
                                                                               "npoint_",
                                                                               "n_point_with_"),
                                                               "_value")

  colnames(out)[stringr::str_detect(colnames(out), "all_")] <- paste0(stringr::str_replace_all(colnames(out)[stringr::str_detect(colnames(out), "all_")],
                                                                             "all_",
                                                                             "All_"),
                                                             "_values")

  colnames(out)[colnames(out) %in% c("shiptype", "name", "length", "draught", "width", "imo")] <- paste0("Selected_",
                                                                                                         colnames(out)[colnames(out) %in% c("shiptype", "name", "length", "draught", "width", "imo")])

  cat("Warnings are printed if any value of length, draught, width or imo is not numeric,
      or any value of shiptype and name can not be transformed to character.\n")

  return(list(estimated_values = out %>%
                dplyr::select(mmsi, dplyr::all_of(colnames(.)[stringr::str_detect(colnames(.), "Selected")])),
              summary = out))

}
