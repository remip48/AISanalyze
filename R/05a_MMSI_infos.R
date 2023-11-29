#' Title
#'
#' @param ais
#' @param threshold_length
#' @param weight_complete_data
#'
#' @return
#' @export
#'
#' @examples
MMSIlength <- function(ais, ## need one column length, one shiptype, and one date
                       threshold_length = 475,
                       weight_complete_data = 10) {

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  weight_complete_data <- as.numeric(weight_complete_data)
  weight_complete_data <- ifelse(is.na(weight_complete_data) | is.nan(weight_complete_data) | is.null(weight_complete_data),
                                 1, weight_complete_data)

  temp <- ais %>%
    dplyr::mutate(shiptype = ifelse(str_remove_all(shiptype, " ") == "", NA, shiptype)) %>%
    group_by(mmsi, shiptype, length) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    dplyr::mutate(length = as.numeric(length),
                  length = ifelse(length == 0 | length > threshold_length, NA, length),
                  real_n = n(),
                  n = ifelse(!is.na(length) & !is.na(shiptype), n*weight_complete_data, n)) %>%
    as.data.frame()

  out <- temp %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint = sum(real_n),
                     npoint_all_lengths = sum(real_n[!is.na(as.numeric(length))]),
                     npoint_all_lengths_weighted = sum(n[!is.na(as.numeric(length))]),
                     all_lengths = paste(sort(unique(na.omit(as.numeric(length)))), collapse = ", "),
                     n_length = length(unique(na.omit(as.numeric(length)))),
                     # all_dates_length = paste(unique(date[!is.na(as.numeric(length))]), collapse = ", "),
                     length = all_lengths,
                     npoint_length = npoint_all_lengths,
                     npoint_length_weighted = npoint_all_lengths_weighted) %>%
    ungroup()

  if (any(out$n_length > 1)) {

    to_corr <- out %>%
      dplyr::filter(n_length > 1)

    uni_length <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$length), ] %>%
      group_by(mmsi, length) %>%
      dplyr::summarise(n_l = sum(n),
                       n_li = sum(real_n)
                       # dates = paste(unique(date[!is.na(as.numeric(length))]), collapse = ", ")
                       ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(length = length[which.max(n_l)],
                       # dates_length = paste(unique(date[which.max(n_l)]), collapse = ", "),
                       npoint_length = n_li[which.max(n_l)],
                       npoint_length_weighted = max(n_l)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(length, npoint_length, npoint_length_weighted)) %>% # dates_length
      left_join(uni_length, by = "mmsi") %>%
      mutate(length = as.numeric(length),
             npoint_length = as.numeric(npoint_length),
             npoint_length_weighted = as.numeric(npoint_length_weighted))

    out <- map_dfr(list(out %>%
                          dplyr::filter(n_length < 2) %>%
                          mutate(length = as.numeric(length),
                                 npoint_length = as.numeric(npoint_length),
                                 npoint_length_weighted = as.numeric(npoint_length_weighted)),
                        to_corr), function(f) {return(f)})

  } else {
    out <- out %>%
      mutate(length = as.numeric(length),
             npoint_length = as.numeric(npoint_length),
             npoint_length_weighted = as.numeric(npoint_length_weighted))
  }

  out_type <- temp %>%
    mutate(shiptype = ifelse(tolower(str_remove_all(shiptype, " ")) %in% c("undefined", "unknown", "", "na"), NA, shiptype)) %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint_all_type = sum(real_n[!is.na(shiptype)]),
                     npoint_all_type_weighted = sum(n[!is.na(shiptype)]),
                     all_types = paste(sort(unique(na.omit(shiptype))), collapse = ", "),
                     n_type = length(unique(na.omit(shiptype))),
                     # all_dates_type = paste(unique(date[!is.na(shiptype)]), collapse = ", "),
                     type = all_types,
                     npoint_type = npoint_all_type,
                     npoint_type_weighted = npoint_all_type_weighted) %>% #,
    ungroup()

  if (any(out_type$n_type > 1)) {

    to_corr <- out_type %>%
      dplyr::filter(n_type > 1)

    uni_type <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$shiptype), ] %>%
      mutate(shiptype = ifelse(tolower(str_remove_all(shiptype, " ")) %in% c("undefined", "unknown", "", "na"), NA, shiptype)) %>%
      dplyr::filter(!is.na(shiptype)) %>%
      group_by(mmsi, shiptype) %>%
      dplyr::summarise(n_t = sum(n),
                       n_ti = sum(real_n)
                       # date = paste(unique(date[!is.na(shiptype)]), collapse = ", ")
                       ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(type = shiptype[which.max(n_t)],
                       # dates_type = paste(unique(date[which.max(n_t)]), collapse = ", "),
                       npoint_type = n_ti[which.max(n_t)],
                       npoint_type_weighted = max(n_t)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(type, npoint_type, npoint_type_weighted)) %>%
      left_join(uni_type, by = "mmsi")

    out_type <- map_dfr(list(out_type %>%
                               dplyr::filter(n_type < 2),
                             to_corr), function(f) {return(f)})

  }

  out <- out %>%
    left_join(out_type, by = "mmsi") %>%
    mutate(type = ifelse(type == "", NA, type))

  return(out)

}
