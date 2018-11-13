
#' Score Linkage Variables
#'
#' @param df site MSD file
#'
#' @importFrom magrittr %>%
#' @export


score_lnk <- function(df){
  #1. Proxy Linkage

  #pull headers to figure out last three quarters to keep
    headers <- names(df_site)
    pds <- headers[stringr::str_detect(headers, "q(?=[:digit:])")] %>%
      tail(., n =3)

  #narrow down to data needed for indicator creation
    lnk_val <- df_site %>%
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, indicator, pds) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains("q")), dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing

  #create linkage value
    lnk_val <- lnk_val %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy")) %>%
      dplyr::select(-pd) %>%
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(indicator, val) %>%
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS,
             linkage = ifelse(!is.finite(linkage), 0, linkage))

  #Calculate percentile grouping
    lnk_val <- lnk_val %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(lnk.proxylinkage.score =
                      dplyr::case_when(linkage < quantile(linkage, .25) ~ 2,
                                       linkage < quantile(linkage, .50) ~ 1,
                                       TRUE                             ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(lnk.proxylinkage.value = linkage) %>%
      dplyr::select(-HTS_TST_POS, -TX_NEW)


  #4. & 5. Number of quarters without decrease/increase
  #narrow down to data needed for indicator creation
    lnk_chng <- df_site %>%
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
                    standardizeddisaggregate == "Total Numerator",
                    typemilitary == "N") %>%
      dplyr::select(operatingunit, psnu,sitename, orgunituid, indicator, dplyr::contains("q")) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains("q")), dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing

  #create linkage value
    lnk_chng <- lnk_chng %>%
      #get all indicator values on same rows
      dplyr::group_by_if(is.character) %>%
      dplyr::summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::gather(pd, val, dplyr::starts_with("fy")) %>%
      tidyr::spread(indicator, val) %>%
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS,
                    linkage = ifelse(!is.finite(linkage), 0, linkage))
  #change
    lnk_chng <- lnk_chng %>%
      dplyr::group_by(operatingunit, psnu, sitename, orgunituid) %>%
      dplyr::mutate(link_qtrdiff_pos = ifelse(linkage - lag(linkage) > 0, 1, 0),
                    link_qtrdiff_neg = ifelse(linkage - lag(linkage) < 0, 1, 0))%>%
      dplyr::ungroup()

    lnk_chng <- lnk_chng %>%
      dplyr::group_by(operatingunit, psnu, sitename, orgunituid) %>%
      dplyr::summarise_at(dplyr::vars(dplyr::contains("link")), ~ sum(., na.rm = TRUE)) %>%
      dplyr::ungroup()

    lnk_chng <- lnk_chng %>%
      dplyr::group_by(operatingunit) %>%
      dplyr::mutate(lnk.lnk_pos_chng.score =
                      dplyr::case_when(link_qtrdiff_pos > quantile(link_qtrdiff_pos, .75) ~ 2,
                                       link_qtrdiff_pos > quantile(link_qtrdiff_pos, .50) ~ 1,
                                       TRUE                                               ~ 0),
                    lnk.lnk_neg_chng.score =
                      dplyr::case_when(link_qtrdiff_neg > quantile(link_qtrdiff_neg, .75) ~ 2,
                                       link_qtrdiff_neg > quantile(link_qtrdiff_neg, .50) ~ 1,
                                       TRUE                                               ~ 0)) %>%
      dplyr::ungroup() %>%
      dplyr::rename(lnk.lnk_pos_chng.value = link_qtrdiff_pos,
             lnk.lnk_neg_chng.value = link_qtrdiff_neg) %>%
      dplyr::select(-linkage)

  #combine all together
    lnk <- dplyr::full_join(lnk_chng, lnk_val, by = c("operatingunit", "psnu", "sitename", "orgunituid"))
    return(lnk)

}


