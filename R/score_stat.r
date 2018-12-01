
#' Score HIV Status Indicators
#'
#' @param df site MSD file
#'
#' @importFrom magrittr %>%
#' @export


score_stat <- function(df){

  #1. OVC Known Status
  if(all((c("OVC_HIVSTAT", "OVC_SERV") %in% unique(df$indicator))) == TRUE){
    stat_ovc <- df %>%
      dplyr::filter(indicator %in% c("OVC_HIVSTAT", "OVC_SERV"),
             (standardizeddisaggregate == "Total Numerator" |
                (standardizeddisaggregate == "ReportedStatus" & otherdisaggregate ==  "No HIV Status"))) %>%
      ICPIutilities::add_cumulative() %>%
      dplyr::mutate(standardizeddisaggregate = stringr::str_remove(standardizeddisaggregate, " ")) %>%
      tidyr::unite(ind, indicator, standardizeddisaggregate) %>%
      dplyr::group_by(operatingunit, psnu, sitename, orgunituid, ind) %>%
      dplyr::summarise_at(dplyr::vars(fy2018cum), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(ind, fy2018cum) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains("OVC")), dplyr::any_vars(!is.na(.) & .!=0)) %>%
      dplyr::mutate(knownstatus = round(1-(OVC_HIVSTAT_ReportedStatus/OVC_HIVSTAT_TotalNumerator), 2)) %>%
      dplyr::select(operatingunit, psnu, sitename, orgunituid, ovc_serv = OVC_SERV_TotalNumerator, knownstatus)

    stat_ovc <- stat_ovc %>%
      dplyr::mutate(stat.ovc.score =
                      dplyr::case_when(ovc_serv > median(ovc_serv, na.rm = TRUE) &
                                       knownstatus < median(knownstatus, na.rm = TRUE)     ~ 2,
                                       knownstatus < median(knownstatus, na.rm = TRUE)     ~ 1,
                                       TRUE                                                ~ 0)) %>%
      dplyr::rename(stat.ovc.value = knownstatus) %>%
      dplyr::select(-ovc_serv)
  }

  if(any((c("PMTCT_STAT", "TB_STAT") %in% unique(df$indicator))) == TRUE){
  #2. & 3. PMTCT & TB Known Status
    stat_oth <- df %>%
      dplyr::filter(indicator %in% c("PMTCT_STAT", "TB_STAT"),
                    stringr::str_detect(standardizeddisaggregate, "Total")) %>%
      ICPIutilities::add_cumulative() %>%
      dplyr::group_by(operatingunit, psnu, sitename, orgunituid, indicator, numeratordenom) %>%
      dplyr::summarise_at(dplyr::vars(fy2018cum), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(fy2018cum != 0) %>%
      tidyr::spread(numeratordenom, fy2018cum) %>%
      dplyr::mutate(knownstatus = round(N / D, 2)) %>%
      dplyr::select(-N)

    stat_oth <- stat_oth %>%
      dplyr::mutate(score =
                      dplyr::case_when(D > median(D, na.rm = TRUE) &
                                       knownstatus < median(knownstatus, na.rm = TRUE)     ~ 2,
                                       knownstatus < median(knownstatus, na.rm = TRUE)     ~ 1,
                                       TRUE                                                ~ 0)) %>%
      dplyr::rename(value = knownstatus) %>%
      dplyr::select(-D) %>%
      dplyr::mutate(indicator = stringr::str_extract(indicator, "(PMTCT|TB)") %>% paste0("stat.", .) %>% tolower()) %>%
      tidyr::gather(type, val, value, score) %>%
      tidyr::unite(ind, indicator, type, sep = ".") %>%
      tidyr::spread(ind, val)
  }

  #join together
  if(all(exists(c("stat_ovc", "stat_oth")))){
    stat <- dplyr::full_join(stat_ovc, stat_oth, by = c("operatingunit", "psnu", "sitename", "orgunituid"))
  }


  #return
  if(exists("stat")) {
    return(stat)
  } else if(exists("stat_ovc")) {
    stat <- stat_ovc
    return(stat)
  } else if(exists("stat_oth")) {
    stat <- stat_oth
    return(stat)
  } else {
    print("no stat variables")
  }

}
