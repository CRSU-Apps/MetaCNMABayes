required_binary_columns <- c(
  "Study",
  "Components",
  "Events",
  "Total"
)

required_continuous_columns <- c(
  "Study",
  "Components",
  "Mean",
  "SD",
  "Total"
)

validate_columns <- function(df, data_type) {
  required_columns <- NULL
  column_names <- names(df)
  if (data_type == "binary")  {
    required_columns <- required_binary_columns
  } else if (data_type == "continuous") {
    required_columns <- required_continuous_columns
  } else {
    stop("An internal error occurred")
  }
  lapply(required_columns, function(name) {
    if (!tolower(name) %in% tolower(column_names)) {
      stop(paste0("Error column ", name, " is missing from the data"))
    } else {
      # Do Nothing
    }
  })
}
