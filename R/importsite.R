
#' Import Site MSD
#'
#' @param msd_filepath full filepath for MSD site file
#'
#' @export


importsite <- function(msd_filepath){

  #read in file
    if(tools::file_ext(msd_filepath) == "rds"){
      df_site <- readr::read_rds(msd_filepath)
    } else if(tools::file_ext(msd_filepath) == "txt") {
      df_site <- ICPIutilities::read_msd(msd_filepath, save_rds = FALSE)
    } else {
      stop("Check that file exist or file exention is correct (need .rds or .txt")
    }
}
