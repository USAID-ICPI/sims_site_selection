#' Subset site dataset to just relevant indicators
#'
#' @param df full filepath for MSD site file
#'
#' @importFrom magrittr %>%
#' @export

limit <- function(df){

  #funding agencies - limit to use CDC and USAID
    df_lim <- dplyr::filter(df, fundingagency %in% c("USAID", "HHS/CDC"))

  #remove military(shouldn't exist without Mil) & above site
    df_lim <- df_lim %>%
      dplyr::filter(typemilitary == "N",
                    sitetype != "NULL")

  #remove blank rows
    df_lim %>%
      dplyr::select(-fy2019_targets) %>%
      dplyr::filter_at(dplyr::vars(dplyr::matches("q|targets")), dplyr::any_vars(!is.na(.) & .!=0)) #remove if all quarters are missing

  #key indicators & disaggs
    df_ind <- tibble::tribble(
            ~indicator,            ~standardizeddisaggregate,
             "HTS_TST",                    "Total Numerator",
             "HTS_TST", "Modality/Age Aggregated/Sex/Result",
             "HTS_TST",            "Modality/Age/Sex/Result",
         "HTS_TST_POS",                    "Total Numerator",
             "KP_PREV",                    "Total Numerator",
              "TX_NEW",                    "Total Numerator",
          "TX_NET_NEW",                    "Total Numerator",
            "OVC_SERV",                    "Total Numerator",
         "OVC_HIVSTAT",                    "Total Numerator",
         "OVC_HIVSTAT",                     "ReportedStatus",
          "PMTCT_STAT",                    "Total Numerator",
          "PMTCT_STAT",                  "Total Denominator",
             "TB_STAT",                    "Total Numerator",
             "TB_STAT",                  "Total Denominator",
           "VMMC_CIRC",                    "Total Numerator"
         )

  #inner join to just keep
    df_lim <- dplyr::inner_join(df_lim, df_ind, by = c("indicator", "standardizeddisaggregate"))



}
