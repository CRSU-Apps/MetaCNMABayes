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

get_all_components <- function(components_column) {
  components <- levels(as.factor(components_column))
  components <- paste(components, collapse = "+")
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
}

get_unique_components <- function(components_column) {
  components <- get_all_components(components_column)
  return(
    # Use levels to get unique components
    components <- levels(components)
  )
}

get_most_frequent_component <- function(components_column) {
  components <- get_all_components(df$Components)
  component_frequency <- data.frame(table(components))
  most_frequent_component <-
    component_frequency[which.max(component_frequency$Freq), ]$components
  return(most_frequent_component)
}

format_data <- function(df, control = NULL) {
  # Convert columns to lowercase
  names(df) <- tolower(names(df))
  n_trials <- length(levels(as.factor(df$study)))
  if (is.null(control)) {
    control <- get_most_frequent_component(df$components)
  }

}

format_data_binary <- function(df) {

}

format_data_continous <- function(df) {

}

get_n_arms <- function(df) {
  require(dplyr)
  tmp_data <- df
  tmp_data <- tmp_data %>%
    dplyr::mutate(study = as.factor(study)) %>%
    dplyr::group_by(study) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::select(study, n) %>%
    dplyr::filter(row_number() == 1)
}

get_n_array <- function(data, control, column = "events") {

}