
#' Identify Site Meta Data
#'
#' @param df site MSD file
#'
#' @importFrom magrittr %>%
#' @export


identifysites <- function(df){

  #create placeholder df to ensure both agencies are all OUs datasets (for reshape)
    df_plc <- tibble::tribble(  ~orgunituid, ~fundingagency,
                                "PLACEHOLDER",        "USAID",
                                "PLACEHOLDER",      "HHS/CDC")

  #site info with agency affiliations
    sites <- df %>%
      dplyr::filter(fundingagency %in% c("HHS/CDC", "USAID")) %>%
      dplyr::bind_rows(df_plc) %>% #add in placeholders to ensure both USAID + CDC are present in all
      dplyr::distinct(sitename, sitetype, operatingunit, psnu, snuprioritization, orgunituid, fundingagency) %>%

      dplyr::mutate(n = "X", #identify sites with agency presence with X
                    fundingagency = stringr::str_remove(fundingagency, "(HHS/|/AF)") %>%
                      paste0("agency_", .) %>%
                      tolower()) %>%
      tidyr::spread(fundingagency, n) %>%
      dplyr::filter(orgunituid != "PLACEHOLDER")

    return(sites)
}
