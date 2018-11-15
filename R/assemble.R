
#' Assemble scores by site
#'
#' @param filepath full filepath for MSD site file
#' @param template_filepath where is the SIMS template file located?
#' @param output_folderpath what folder do you want the output saved to?
#'
#' @export
#' @importFrom magrittr %>%


assemble <- function(filepath, template_filepath = NULL, output_folderpath = NULL){

  cat("\n\nread in data ... ")

  #import
    df_import <- importsite(filepath)

  ou <- df_import %>% dplyr::distinct(operatingunit) %>% dplyr::pull()
  cat(ou, " ... score sites ...")

  #subset to relevant agencies and indicators
    df_site <- limit(df_import)

  #identify site list and information
    df_meta <- identifysites(df_site)

  #create scores
    df_ci <- score_ci(df_site)
    df_init <- score_init(df_site)
    # df_lnk <- score_lnk(df_site)
    df_prfm <- score_prfm(df_site)
    df_stat <- score_stat(df_site)

  #combine all together
    df_combo <- list(df_meta, df_ci, df_init, #df_lnk,
                     df_prfm, df_stat) %>%
      purrr::reduce(dplyr::full_join, by = c("operatingunit", "psnu", "sitename", "orgunituid"))

  #remove blank rows & arrange by TX_NEW volumne
    df_export <- df_combo %>%
      dplyr::filter_at(dplyr::vars(dplyr::matches("^(ci|init|lnk|stat|prfm)")),
                       dplyr::any_vars(!is.na(.) & .!=0)) %>%
      dplyr::arrange(desc(init.tx_new_ou.value))

  #reshape wide so scores and values are on their own rows
    df_export <- convert_wide(df_export)

  #fill gaps if any indicators are missing
    df_export <- align_output(df_export)

  #export (if template location is specified)
    if(!is.null(template_filepath)){

      #save to wd if no output path is specified
      if(is.null(output_folderpath)) output_folderpath <- getwd()

      #filter to just score and reoder to match template layout
      df_export <- df_export %>%
        dplyr::filter(type == "score")

      #export to template
      fill_template(df_export, template_filepath, output_folderpath)
    }

    cat(" ... complete")

}
