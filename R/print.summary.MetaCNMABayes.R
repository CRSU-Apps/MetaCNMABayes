#' Default print.summary function for MetaCNMABayes
#'
#' @param obj An object of type MetaCNMABayes
#' @param ...
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' model_fit <- fit_binary_fe(binary, "CONTROL")
#' print.summary(model_fit)
#' }
print.summary.MetaCNMABayes <- function(obj, ...) {
  if (class(obj) != "MetaCNMABayes") {
    stop("Object must be of class MetaCNMABayes")
  }
  if (obj$data_type == "binary") {
    binary_print_summary(obj, ...)
  } else if (obj$data_type == "continuous") {
    continuous_print_summary(obj, ...)
  } else {
    print("Unknown data type")
  }
  invisible()
}

binary_print_summary <- function(obj, ...) {

}

continuous_print_summary <- function(obj, ...) {
    
}