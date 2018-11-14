
#' Score Case Identification Indicators
#'
#' @param df site MSD file
#'
#' @export
#' @importFrom magrittr %>%


score_ci <- function(df){

  #1.&2. HTS_TST_POS Position relative to OU-/PSNU-level median (median last three quarters)

  #pull headers to figure out last three quarters to keep
    headers <- names(df)
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>%
      tail(., n =3)

  #narrow down to data needed for indicator creation
    ci_hts_pos <- df %>%
      dplyr::filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate == "Total Numerator",
             typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds) %>%
      dplyr::filter_at(dplyr::vars( dplyr::contains("q")),
                        dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  #site sum over last 3 pds
    ci_hts_pos <- ci_hts_pos %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy")) %>%
      dplyr::select(-pd) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup()

  #Calculate percentile grouping
    ci_hts_pos <- ci_hts_pos %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(ci.hts_pos_ou.score = dplyr::case_when(val > quantile(val, .75) ~ 2,
                                             val > quantile(val, .50) ~ 1,
                                             TRUE                     ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(operatingunit, psnu) %>%
      dplyr::mutate(ci.hts_pos_psnu.score = dplyr::case_when(val > quantile(val, .75) ~ 2,
                                               val > quantile(val, .50) ~ 1,
                                               TRUE                     ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ci.hts_pos_ou.value = val) %>%
      dplyr::rename(ci.hts_pos_psnu.value = val)

  #3. Year on Year change in volume

  #figure out quarter to cal cumulative in prior year
    qtr <- ICPIutilities::identifypd(df, "quarter")
    qtr_fltr <- dplyr::case_when(qtr == 1 ~ "1",
                          qtr == 2 ~ "(1|2)",
                          qtr == 3 ~ "(1|2|3)",
                          qtr == 4 ~ "(?=[:digit:])")
    pds <- headers[stringr::str_detect(headers, paste0("q", qtr_fltr))]

  #filter to key cols and variables
    ci_hts_pos_yoy <- df %>%
      dplyr::filter(indicator == "HTS_TST_POS",
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds)

  #create year over year percent/raw change
    ci_hts_pos_yoy <- ci_hts_pos_yoy %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy"), na.rm = TRUE) %>%
      dplyr::mutate(pd = stringr::str_remove_all(pd, "q[:digit:]")) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::mutate(ci.hts_pos_yoyd =  val - dplyr::lag(val),
                    ci.hts_pos_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(pd == "fy2018", is.finite(ci.hts_pos_yoyc))

  #Calculate percentile grouping
  ci_hts_pos_yoy <- ci_hts_pos_yoy %>%
    dplyr::group_by(operatingunit) %>%
    dplyr::mutate(ci.hts_pos_yoyd.score =
                    dplyr::case_when(ci.hts_pos_yoyd > quantile(ci.hts_pos_yoyd, .75) ~ 2,
                                     ci.hts_pos_yoyd > quantile(ci.hts_pos_yoyd, .50) ~ 1,
                                     TRUE                                             ~ 0),
                  ci.hts_pos_yoyc.score =
                    dplyr::case_when(ci.hts_pos_yoyc > quantile(ci.hts_pos_yoyc, .75) ~ 2,
                                     ci.hts_pos_yoyc > quantile(ci.hts_pos_yoyc, .50) ~ 1,
                                     TRUE                                             ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(ci.hts_pos_yoyd.value = ci.hts_pos_yoyd,
           ci.hts_pos_yoyc.value = ci.hts_pos_yoyc,
           ci.hts_pos_yoy.value = val) %>%
    dplyr::select(-pd, -ci.hts_pos_yoy.value)

  #4. Share Index testing

  #narrow down to data needed for indicator creation
    ci_index <- df %>%
      dplyr::filter(indicator == "HTS_TST",
                    standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                                    "Modality/Age/Sex/Result"),
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds, modality) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains("q")), dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing

  #site sum over last 3 pds
    ci_index <- ci_index %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy"), na.rm = TRUE) %>%
      dplyr::select(-pd) %>%
      dplyr::mutate(modality = ifelse(modality %in% c("Index", "IndexMod"), "ALL Index", modality)) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup()

  #index share
    ci_index <- ci_index %>%
      dplyr::group_by(operatingunit, psnu, sitename, orgunituid) %>%
      dplyr::mutate(indexshare = round(val / sum(val), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(modality == "ALL Index") %>%
      dplyr::select(-modality)

  #Calculate percentile grouping
    ci_index <- ci_index %>%
      dplyr::group_by(operatingunit, psnu) %>%
      dplyr::mutate(ci.hts_index.score =
                      dplyr::case_when(indexshare < quantile(indexshare, .25) ~ 2,
                                       indexshare < quantile(indexshare, .50) ~ 1,
                                       TRUE                                   ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(ci.hts_index.value = indexshare) %>%
      dplyr::select(-val)

  #combine all together
    ci <- list(ci_hts_pos, ci_hts_pos_yoy, ci_index) %>%
      purrr::reduce(dplyr::full_join, by = c("operatingunit", "psnu", "sitename", "orgunituid"))
    return(ci)
}


