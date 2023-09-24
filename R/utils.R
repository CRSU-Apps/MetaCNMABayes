#' data_wide_to_long
#'
#' @description
#' Internal function for converting wide format data to long format
#'
#'
#' @param df data.frame, to be converted from wide to long
#'
#' @return data.frame, converted from wide to long
#'
#' @examples
data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}

#' is_wide
#'
#' @description
#' Internal function to check if a data.frame is in a wide format
#'
#'
#' @param df data.frame, to be checked if in wide format
#'
#' @return logical, true if in wide format
#'
#' @examples
is_wide <- function(df) {
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}
