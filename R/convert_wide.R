#' Reshape wide
#'
#' @param df dataframe to convert wide
#'
#' @importFrom magrittr %>%
#' @export

convert_wide <- function(df){

  df %>%
    tidyr::gather(metric, val, dplyr::matches("^(ci|init|lnk|stat|prfm)"), na.rm = TRUE) %>%
    tidyr::separate(metric, c("grp", "metric", "type"), sep = "\\.") %>%
    tidyr::unite(metric, grp, metric, sep = ".") %>%
    tidyr::spread(metric, val)
}
