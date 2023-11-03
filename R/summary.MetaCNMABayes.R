#' Default summary function for MetaCNMABayes
#'
#' @param obj An object of type MetaCNMABayes
#' @param ...
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' model_fit <- fit_binary_fe(binary, "CONTROL")
#' summary(model_fit)
#' }
summary.MetaCNMABayes <- function(obj, ...) {
  if (class(obj) != "MetaCNMABayes") {
    stop("Object must be of class MetaCNMABayes")
  }
  if (obj$data_type == "binary") {
    binary_summary(obj, ...)
  } else if (obj$data_type == "continuous") {
    continuous_summary(obj, ...)
  } else {
    print("Unknown data type")
  }
  invisible()
}

binary_summary <- function(obj, ...) {

}

continuous_summary <- function(obj, ...) {
    
}