#' Write Data To Template
#'
#' @param df dataframe to export
#' @param template_filepath file location of the SIMS Prioritization Tool
#' @param output_folderpath folder location of where you want the output stored
#'
#' @export
#' @importFrom magrittr %>%


fill_template <- function(df, template_filepath, output_folderpath){

  #identify opunit for file name and template headers
    opunit <- df %>%
      dplyr::distinct(operatingunit) %>%
      dplyr::pull()
  #site list for template dropdown
    sites <- df %>%
      dplyr::distinct(sitename, orgunituid) %>%
      dplyr::select(sitename, orgunituid) #reorder

  #open workbook for editing
    wb <- openxlsx::loadWorkbook(template_filepath)

  #paste opunit
    openxlsx::writeData(wb, sheet = "rs", x = opunit,
                        startCol = 5, startRow = 2, colNames = FALSE)

  #paste sites
    openxlsx::writeData(wb, sheet = "rs", x = sites,
                        startCol = 7, startRow = 2, colNames = FALSE)

  #paste data
    openxlsx::writeData(wb, sheet = "rawdata", x = df,
                        startCol = 1, startRow = 3, colNames = FALSE)

  #note date updated
    # when <- paste("Updated:", Sys.Date())
    # openxlsx::write.xlsx(wb, sheet = "Guidance", x = when,
    #                      startCol = 2, startRow = 73, colNames = FALSE)


  #hide rs and rawdata tab
    openxlsx::sheetVisibility(wb)[5] <- "hidden"
    openxlsx::sheetVisibility(wb)[6] <- "hidden"

  #save workbook
    name <- file.path(output_folderpath, paste0(opunit, "_SIMS_Prioritization_",
                                       stringr::str_remove_all(Sys.Date(), "-"), ".xlsx"))

    openxlsx::saveWorkbook(wb, name, overwrite = TRUE)
}
