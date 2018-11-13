
#' Score Performance Indicators
#'
#' @param df site MSD file
#'
#' @export
#' @importFrom magrittr %>%


score_prfm <- function(df){

  prfm_ind <- df_site %>%
    dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "PMTCT_STAT", "OVC_SERV", "KP_PREV", "VMMC_CIRC"),
                  standardizeddisaggregate == "Total Numerator",
                  typemilitary == "N") %>%
    ICPIutilities::add_cumulative() %>%
    dplyr::group_by(operatingunit, psnu,sitename, orgunituid, indicator) %>%
    dplyr::summarise_at(dplyr::vars(fy2018cum, fy2018_targets), ~ sum(., na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(fy2018_targets !=0) %>%
    dplyr::mutate(achievement = round(fy2018cum/fy2018_targets, 3)) %>%
    dplyr::select(-fy2018cum) %>%
    dplyr::mutate(indicator = paste0("prfm.", stringr::str_to_lower(indicator)))

  thres_med <- addthresholds(df_site) %>% .["med"]

  prfm_ind <- prfm_ind %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(score = dplyr::case_when(achievement < thres_med &
                               fy2018_targets > median(fy2018_targets, na.rm = TRUE) ~ 2,
                             achievement < thres_med                                 ~ 1,
                             TRUE                                                    ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(value = achievement) %>%
    dplyr::select(-fy2018_targets) %>%
    tidyr::gather(type, val, value, score, na.rm = TRUE) %>%
    tidyr::unite(ind, indicator, type, sep = ".") %>%
    tidyr::spread(ind, val)

  return(prfm_ind)

}

