#' Estimate vessel type, length, width, draught, imo and name
#'
#' Collect information on the type, length, width, draught, imo and name of the vessels to identify the (probable) real values, among the errors usually present in the AIS columns. Extract the length, shiptype, width, draught, imo and name occurring the most in the MMSI data as the probable real values, and give higher weight to lines with complete data than lines with partial data information. The shiptypes should be checked beforehand to merge the same shiptypes entered as different names. The results should be combined to other AIS data containing this MMSI to confirm returned information returned: analysis showed that daily data are not sufficient to provide reliable values.
#'
#' @param ais_data AIS data. Must contain the columns shiptype, length, width, draught, imo and name and mmsi (Maritime mobile service identity). The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param threshold_length threshold (meters) above which the length is considered as unrealistic (and set as NA).
#' @param weight_complete_data weigth for lines having both the information on length and on type: complete lines are supposed to be more reliable (10 by default for the weight).
#'
#' @return A list with dataframes "values_obtained" and summary. This last present the statistics of analysis per MMSI, and contains the columns:
#' \itemize{
#' \item npoint: total number of AIS receptions for this MMSI.
#' \item npoint_all_lengths: number of AIS reception having a value in the column length for this MMSI.
#' \item npoint_all_lengths_weighted: number of AIS reception (weighted by "weight_complete_data") having a value in the column length for this MMSI.
#' \item all_lengths: all the values of length found in the AIS data for this MMSI.
#' \item n_length: number of values of length found in the AIS data for this MMSI.
#' \item length: estimated length of the MMSI, based on the weighted number of points of each length. The length having the greatest score is considered as the real length.
#' \item npoint_length: number of AIS reception having the value "length" (estimated real length) for this MMSI.
#' \item npoint_length_weighted: number of AIS reception (weighted by "weight_complete_data") having the value "length" (estimated real length) for this MMSI.
#' \item npoint_all_type: number of AIS reception having a value in the column shiptype for this MMSI.
#' \item npoint_all_type_weighted: number of AIS reception (weighted by "weight_complete_data") having a value in the column shiptype for this MMSI.
#' \item all_types: all the values of shiptype found in the AIS data for this MMSI.
#' \item n_type: number of values of shiptype found in the AIS data for this MMSI.
#' \item type: estimated shiptype of the MMSI, based on the weighted number of points of each shiptype. The shiptype having the greatest score is considered as the real shiptype
#' \item npoint_type: number of AIS reception having the value "type" (estimated real shiptype) for this MMSI.
#' \item npoint_type_weighted: number of AIS reception (weighted by "weight_complete_data") having the value "type" (estimated real shiptype) for this MMSI.}
#'
#' The same columns are present with the "width", "draught", "imo", "name" replacing "length" of "type" in the column name.
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' MMSI_infos(ais_data = ais,
#'            threshold_length = 475,
#'            weight_complete_data = 10)}
#' @export

MMSI_infos <- function(ais_data,
                       threshold_length = 475,
                       threshold_draught = NA,
                       threshold_width = NA,
                       weight_complete_data = 10) {

  weight_complete_data <- as.numeric(weight_complete_data)
  weight_complete_data <- ifelse(is.na(weight_complete_data) | is.nan(weight_complete_data) | is.null(weight_complete_data),
                                 1, weight_complete_data)

  temp <- ais %>%
    dplyr::mutate(shiptype = ifelse(str_remove_all(shiptype, " ") == "", NA, shiptype)) %>%
    dplyr::mutate(shiptype = ifelse(str_remove_all(name, " ") == "", NA, shiptype)) %>%
    group_by(mmsi, shiptype, length, width, draught, imo, name) %>%
    dplyr::summarise(n = n()) %>%
    ungroup() %>%
    dplyr::mutate(length = as.numeric(length),
                  draught = as.numeric(draught),
                  width = as.numeric(width),
                  imo = as.numeric(imo),
                  shiptype = as.character(shiptype),
                  name = as.character(name),
                  length = ifelse(length == 0 | length > threshold_length, NA, length),
                  draught = ifelse(draught == 0 | draught > threshold_draught, NA, draught),
                  width = ifelse(width == 0 | width > threshold_width, NA, width),
                  real_n = n,
                  n = ifelse(!is.na(length) & !is.na(shiptype)
                             & !is.na(width) & !is.na(draught)
                             & !is.na(imo) & !is.na(name),
                             n*weight_complete_data * 2,
                             ifelse(!is.na(length) & !is.na(shiptype)
                                    & !is.na(width) & !is.na(draught), n*weight_complete_data,
                                    ifelse(any(!do.call("c", map(list(length,
                                                                      width,
                                                                      draught),
                                                                 is.na))) & !is.na(shiptype),
                                           n*weight_complete_data / 2, n)))) %>%
    as.data.frame() %>%
    dplyr::mutate(shipname = name)

  ## for length
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

  out_length <- out

  ## for type
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

  # for draught
  out <- temp %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint_all_draughts = sum(real_n[!is.na(as.numeric(draught))]),
                     npoint_all_draughts_weighted = sum(n[!is.na(as.numeric(draught))]),
                     all_draughts = paste(sort(unique(na.omit(as.numeric(draught)))), collapse = ", "),
                     n_draught = length(unique(na.omit(as.numeric(draught)))),
                     # all_dates_draught = paste(unique(date[!is.na(as.numeric(draught))]), collapse = ", "),
                     draught = all_draughts,
                     npoint_draught = npoint_all_draughts,
                     npoint_draught_weighted = npoint_all_draughts_weighted) %>%
    ungroup()

  if (any(out$n_draught > 1)) {

    to_corr <- out %>%
      dplyr::filter(n_draught > 1)

    uni_draught <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$draught), ] %>%
      group_by(mmsi, draught) %>%
      dplyr::summarise(n_l = sum(n),
                       n_li = sum(real_n)
                       # dates = paste(unique(date[!is.na(as.numeric(draught))]), collapse = ", ")
      ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(draught = draught[which.max(n_l)],
                       # dates_draught = paste(unique(date[which.max(n_l)]), collapse = ", "),
                       npoint_draught = n_li[which.max(n_l)],
                       npoint_draught_weighted = max(n_l)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(draught, npoint_draught, npoint_draught_weighted)) %>% # dates_draught
      left_join(uni_draught, by = "mmsi") %>%
      mutate(draught = as.numeric(draught),
             npoint_draught = as.numeric(npoint_draught),
             npoint_draught_weighted = as.numeric(npoint_draught_weighted))

    out <- map_dfr(list(out %>%
                          dplyr::filter(n_draught < 2) %>%
                          mutate(draught = as.numeric(draught),
                                 npoint_draught = as.numeric(npoint_draught),
                                 npoint_draught_weighted = as.numeric(npoint_draught_weighted)),
                        to_corr), function(f) {return(f)})

  } else {
    out <- out %>%
      mutate(draught = as.numeric(draught),
             npoint_draught = as.numeric(npoint_draught),
             npoint_draught_weighted = as.numeric(npoint_draught_weighted))
  }

  out_draught <- out

  ## for width
  out <- temp %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint_all_widths = sum(real_n[!is.na(as.numeric(width))]),
                     npoint_all_widths_weighted = sum(n[!is.na(as.numeric(width))]),
                     all_widths = paste(sort(unique(na.omit(as.numeric(width)))), collapse = ", "),
                     n_width = length(unique(na.omit(as.numeric(width)))),
                     # all_dates_width = paste(unique(date[!is.na(as.numeric(width))]), collapse = ", "),
                     width = all_widths,
                     npoint_width = npoint_all_widths,
                     npoint_width_weighted = npoint_all_widths_weighted) %>%
    ungroup()

  if (any(out$n_width > 1)) {

    to_corr <- out %>%
      dplyr::filter(n_width > 1)

    uni_width <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$width), ] %>%
      group_by(mmsi, width) %>%
      dplyr::summarise(n_l = sum(n),
                       n_li = sum(real_n)
                       # dates = paste(unique(date[!is.na(as.numeric(width))]), collapse = ", ")
      ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(width = width[which.max(n_l)],
                       # dates_width = paste(unique(date[which.max(n_l)]), collapse = ", "),
                       npoint_width = n_li[which.max(n_l)],
                       npoint_width_weighted = max(n_l)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(width, npoint_width, npoint_width_weighted)) %>% # dates_width
      left_join(uni_width, by = "mmsi") %>%
      mutate(width = as.numeric(width),
             npoint_width = as.numeric(npoint_width),
             npoint_width_weighted = as.numeric(npoint_width_weighted))

    out <- map_dfr(list(out %>%
                          dplyr::filter(n_width < 2) %>%
                          mutate(width = as.numeric(width),
                                 npoint_width = as.numeric(npoint_width),
                                 npoint_width_weighted = as.numeric(npoint_width_weighted)),
                        to_corr), function(f) {return(f)})

  } else {
    out <- out %>%
      mutate(width = as.numeric(width),
             npoint_width = as.numeric(npoint_width),
             npoint_width_weighted = as.numeric(npoint_width_weighted))
  }

  out_width <- out

  ## for name
  out_name <- temp %>%
    mutate(shipname = ifelse(tolower(str_remove_all(shipname, " ")) %in% c("undefined", "unknown", "", "na"), NA, shipname)) %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint_all_name = sum(real_n[!is.na(shipname)]),
                     npoint_all_name_weighted = sum(n[!is.na(shipname)]),
                     all_names = paste(sort(unique(na.omit(shipname))), collapse = ", "),
                     n_name = length(unique(na.omit(shipname))),
                     # all_dates_name = paste(unique(date[!is.na(shipname)]), collapse = ", "),
                     name = all_names,
                     npoint_name = npoint_all_name,
                     npoint_name_weighted = npoint_all_name_weighted) %>% #,
    ungroup()

  if (any(out_name$n_name > 1)) {

    to_corr <- out_name %>%
      dplyr::filter(n_name > 1)

    uni_name <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$shipname), ] %>%
      mutate(shipname = ifelse(tolower(str_remove_all(shipname, " ")) %in% c("undefined", "unknown", "", "na"), NA, shipname)) %>%
      dplyr::filter(!is.na(shipname)) %>%
      group_by(mmsi, shipname) %>%
      dplyr::summarise(n_t = sum(n),
                       n_ti = sum(real_n)
                       # date = paste(unique(date[!is.na(shipname)]), collapse = ", ")
      ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(name = shipname[which.max(n_t)],
                       # dates_name = paste(unique(date[which.max(n_t)]), collapse = ", "),
                       npoint_name = n_ti[which.max(n_t)],
                       npoint_name_weighted = max(n_t)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(name, npoint_name, npoint_name_weighted)) %>%
      left_join(uni_name, by = "mmsi")

    out_name <- map_dfr(list(out_name %>%
                               dplyr::filter(n_name < 2),
                             to_corr), function(f) {return(f)})

  }

  ## for imo
  out <- temp %>%
    group_by(mmsi) %>%
    dplyr::summarise(npoint_all_imos = sum(real_n[!is.na(as.numeric(imo))]),
                     npoint_all_imos_weighted = sum(n[!is.na(as.numeric(imo))]),
                     all_imos = paste(sort(unique(na.omit(as.numeric(imo)))), collapse = ", "),
                     n_imo = length(unique(na.omit(as.numeric(imo)))),
                     # all_dates_imo = paste(unique(date[!is.na(as.numeric(imo))]), collapse = ", "),
                     imo = all_imos,
                     npoint_imo = npoint_all_imos,
                     npoint_imo_weighted = npoint_all_imos_weighted) %>%
    ungroup()

  if (any(out$n_imo > 1)) {

    to_corr <- out %>%
      dplyr::filter(n_imo > 1)

    uni_imo <- temp[temp$mmsi %in% unique(to_corr$mmsi) & !is.na(temp$imo), ] %>%
      group_by(mmsi, imo) %>%
      dplyr::summarise(n_l = sum(n),
                       n_li = sum(real_n)
                       # dates = paste(unique(date[!is.na(as.numeric(imo))]), collapse = ", ")
      ) %>%
      ungroup() %>%
      group_by(mmsi) %>%
      dplyr::summarise(imo = imo[which.max(n_l)],
                       # dates_imo = paste(unique(date[which.max(n_l)]), collapse = ", "),
                       npoint_imo = n_li[which.max(n_l)],
                       npoint_imo_weighted = max(n_l)) %>%
      ungroup()

    to_corr <- to_corr %>%
      dplyr::select(-c(imo, npoint_imo, npoint_imo_weighted)) %>% # dates_imo
      left_join(uni_imo, by = "mmsi") %>%
      mutate(imo = as.numeric(imo),
             npoint_imo = as.numeric(npoint_imo),
             npoint_imo_weighted = as.numeric(npoint_imo_weighted))

    out <- map_dfr(list(out %>%
                          dplyr::filter(n_imo < 2) %>%
                          mutate(imo = as.numeric(imo),
                                 npoint_imo = as.numeric(npoint_imo),
                                 npoint_imo_weighted = as.numeric(npoint_imo_weighted)),
                        to_corr), function(f) {return(f)})

  } else {
    out <- out %>%
      mutate(imo = as.numeric(imo),
             npoint_imo = as.numeric(npoint_imo),
             npoint_imo_weighted = as.numeric(npoint_imo_weighted))
  }

  out_imo <- out

  ## to merge
  out <- out_length %>%
    left_join(out_type, by = "mmsi") %>%
    left_join(out_draught, by = "mmsi") %>%
    left_join(out_width, by = "mmsi") %>%
    left_join(out_imo, by = "mmsi") %>%
    left_join(out_name, by = "mmsi") %>%
    mutate(type = ifelse(type == "", NA, type),
           name = ifelse(name == "", NA, name))

  return(list(values_obtained = out %>%
                dplyr::select(mmsi, length, type, draught, width, imo, name),
              summary = out))

}

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
                  real_n = n,
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
