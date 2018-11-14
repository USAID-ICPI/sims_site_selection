

#' Score Iniation Indicators
#'
#' @param df site MSD file
#'
#' @export
#' @importFrom magrittr %>%


score_init <- function(df){

  #1.&2. TX_NEW Position relative to OU-/PSNU-level median (median last three quarters)

  #pull headers to figure out last three quarters to keep
    headers <- names(df)
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>%
      tail(., n =3)
  #narrow down to data needed for indicator creation
    init_tx_new <- df %>%
      dplyr::filter(indicator == "TX_NEW",
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds) %>%
      dplyr::filter_if(is.numeric, dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing
  #site sum over last 3 pds
    init_tx_new <- init_tx_new %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy")) %>%
      dplyr::select(-pd) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup()
  #Calculate percentile grouping
    init_tx_new <- init_tx_new %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(init.tx_new_ou.score = dplyr::case_when(val > quantile(val, .75) ~ 2,
                                                            val > quantile(val, .50) ~ 1,
                                                            TRUE                     ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(operatingunit, psnu) %>%
      dplyr::mutate(init.tx_new_psnu.score = dplyr::case_when(val > quantile(val, .75) ~ 2,
                                                              val > quantile(val, .50) ~ 1,
                                                              TRUE                     ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(init.tx_new_ou.value = val) %>%
      dplyr::rename(init.tx_new_psnu.value = val)

  #3. Year on Year change in volume

    qtr <- ICPIutilities::identifypd(df, "quarter")

    qtr_fltr <- dplyr::case_when(qtr == 1 ~ "1",
                                 qtr == 2 ~ "(1|2)",
                                 qtr == 3 ~ "(1|2|3)",
                                 qtr == 4 ~ "(?=[:digit:])")

    pds <- headers[stringr::str_detect(headers, paste0("q", qtr_fltr))]

    init_tx_new_yoy <- df %>%
      dplyr::filter(indicator == "TX_NEW",
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds)

    init_tx_new_yoy <- init_tx_new_yoy %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy"), na.rm = TRUE) %>%
      dplyr::mutate(pd = stringr::str_remove_all(pd, "q[:digit:]")) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::mutate(init.tx_new_yoyd =  val - dplyr::lag(val),
                    init.tx_new_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(pd == "fy2018", is.finite(init.tx_new_yoyc))

  #Calculate percentile grouping
    init_tx_new_yoy <- init_tx_new_yoy %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(init.tx_new_yoyd.score =
                      dplyr::case_when(init.tx_new_yoyd > quantile(init.tx_new_yoyd, .75) ~ 2,
                                       init.tx_new_yoyd > quantile(init.tx_new_yoyd, .50) ~ 1,
                                       TRUE                                               ~ 0),
                    init.tx_new_yoyc.score =
                      dplyr::case_when(init.tx_new_yoyc > quantile(init.tx_new_yoyc, .75) ~ 2,
                                       init.tx_new_yoyc > quantile(init.tx_new_yoyc, .50) ~ 1,
                                       TRUE                                               ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(init.tx_new_yoyd.value = init.tx_new_yoyd,
                    init.tx_new_yoyc.value = init.tx_new_yoyc,
                    init.tx_new_yoy.value = val) %>%
      dplyr::select(-pd, -init.tx_new_yoy.value)

  #4. NET NEW change

    init_tx_netnew_yoy <- df %>%
      dplyr::filter(indicator == "TX_NET_NEW",
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, pds)

    init_tx_netnew_yoy <- init_tx_netnew_yoy %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy"), na.rm = TRUE) %>%
      dplyr::mutate(pd = stringr::str_remove_all(pd, "q[:digit:]")) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::mutate(init.tx_netnew_yoyd =  val - dplyr::lag(val),
                    init.tx_netnew_yoyc = (val - dplyr::lag(val))/dplyr::lag(val)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(pd == "fy2018", is.finite(init.tx_netnew_yoyc))

  #Calculate percentile grouping
    init_tx_netnew_yoy <- init_tx_netnew_yoy %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(init.tx_netnew_yoyd.score =
                      dplyr::case_when(init.tx_netnew_yoyd > quantile(init.tx_netnew_yoyd, .75) ~ 2,
                                       init.tx_netnew_yoyd > quantile(init.tx_netnew_yoyd, .50) ~ 1,
                                       TRUE                                                    ~ 0),
                    init.tx_netnew_yoyc.score =
                      dplyr::case_when(init.tx_netnew_yoyc > quantile(init.tx_netnew_yoyc, .75) ~ 2,
                                       init.tx_netnew_yoyc > quantile(init.tx_netnew_yoyc, .50) ~ 1,
                                       TRUE                                                     ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(init.tx_netnew_yoyd.value = init.tx_netnew_yoyd,
                    init.tx_netnew_yoyc.value = init.tx_netnew_yoyc,
                    init.tx_netnew_yoy.value = val) %>%
      dplyr::select(-pd, -init.tx_netnew_yoy.value)

  #combine all together
    init <- list(init_tx_new, init_tx_new_yoy, init_tx_netnew_yoy) %>%
      purrr::reduce(dplyr::full_join, by = c("operatingunit", "psnu", "sitename", "orgunituid"))
    return(init)
}

