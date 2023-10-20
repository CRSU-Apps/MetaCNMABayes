#' Default plot function for MetaCNMABayes
#'
#' @param obj An object of type MetaCNMABayes
#' @param ...
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' plot(modelfit)
#' }
plot.MetaCNMABayes <- function(obj, ...) {
  if (obj$data_type == "binary") {
    binary_forest_plot(obj, ...)
  } else if (obj$data_type == "continuous") {
    print("Sorry not implemented yet")
  } else {
    print("Unknown data type")
  }
  invisible()
}

binary_forest_plot <- function(obj, ...) {
  components <- obj$components
  stan_summary <- as.data.frame(rstan::summary(obj$fit, pars = c("d"))$summary)

  metafor::forest(
    x = stan_summary$mean,
    ci.lb = stan_summary$`2.5%`,
    ci.ub = stan_summary$`97.5%`,
    slab = components,
    transf = exp,
    xlab = "Odds Ratio",
    refline = 1,
    xlim = c(-6,15),
    at = c(0,  1, 2, 5, 10),
    psize = c(rep(1, length(components))),
    cex = 0.9,
    header = c("Component", "OR (95% CrI)")
  )
}
