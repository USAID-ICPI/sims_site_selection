
#' Align output to fit template
#'
#' @param df dataframe to align
#'
#' @importFrom magrittr %>%
#' @export

align_output <- function(df){

  #list all columns and their ordering
    columns <- c(
      #meta
      "orgunituid", "sitename", "operatingunit", "psnu",
      "snuprioritization", "sitetype", "agency_cdc", "agency_usaid",
      "type",
      #case identification
      "ci.hts_index", "ci.hts_pos_ou", "ci.hts_pos_psnu", "ci.hts_pos_yoyc",
      "ci.hts_pos_yoyd", "init.tx_netnew_yoyc",
      #initiation
      "init.tx_netnew_yoyd", "init.tx_new_ou", "init.tx_new_psnu",
      "init.tx_new_yoyc", "init.tx_new_yoyd",
      #linkage
      # "lnk.lnk_neg_chng", "lnk.lnk_pos_chng", "lnk.proxylinkage",
      #performance
      "prfm.hts_tst_pos", "prfm.kp_prev", "prfm.ovc_serv", "prfm.pmtct_stat",
      "prfm.tx_new", "prfm.vmmc_circ",
      #status
      "stat.ovc", "stat.pmtct", "stat.tb"
    )

  #combine all columns into empty df and then bind on df to ensure all cols are there
    df_structured <- purrr::map_dfr(.x = columns,
                                    .f = ~ tibble::tibble(!!.x := logical())) %>%
                     dplyr::bind_rows(df)

  return(df_structured)

}
