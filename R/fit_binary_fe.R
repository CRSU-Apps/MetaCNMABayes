fit_binary_fe <- function(data, control = NULL) {
  if (! "study" %in% tolower(names(data))) {
    stop("The column study is missing from the data")
  }
  df <- data
  if (is_wide(data)) {
    df <- data_wide_to_long(data)
  }
  validate_columns(df, "binary")

}
