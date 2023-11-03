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
#' model_fit <- fit_binary_fe(binary, "CONTROL")
#' plot(model_fit)
#' }
plot.MetaCNMABayes <- function(obj, ...) {
  if (class(obj) != "MetaCNMABayes") {
    stop("Object must be of class MetaCNMABayes")
  }
  if (obj$data_type == "binary") {
    binary_forest_plot(obj, ...)
  } else if (obj$data_type == "continuous") {
    continuous_forest_plot(obj, ...)
  } else {
    print("Unknown data type")
  }
  invisible()
}

meta_forest_plot <- function(
  stan_summary,
  components,
  outcome_measure,
  xlim,
  refline,
  ...
) {

  args <- list(...)

  if (!hasArg(xlab)) {
    xlab <- outcome_measure
  } else {
    xlab <- args[["xlab"]]
  }

  if (is.null(xlim)) {
    xlim <- get_xlim(stan_summary)
  }

  x_lim <- c(xlim[1] - 5, xlim[2] + 5)

  if (!hasArg(ticks)) {
    ticks <- seq.int(from = xlim[1], to = xlim[2])
  } else {
    ticks <- args[["ticks"]]
  }

  if (hasArg(digits)) {
    digits <- args[["digits"]]
  } else if (round(min(abs(stan_summary$mean)), 2) == 0) {
    digits <- 3L
  } else {
    digits <- 2L
  }

  if (!hasArg(transform_function)) {
    transform_function <- function(x) {
      x
    }
  } else {
    transform_function <- args[["transform_function"]]
  }

  metafor::forest(
    x = stan_summary$mean,
    ci.lb = stan_summary$`2.5%`,
    ci.ub = stan_summary$`97.5%`,
    slab = components,
    transf = transform_function,
    xlab = xlab,
    refline = refline,
    xlim = x_lim,
    at = ticks,
    psize = c(rep(1, length(components))),
    cex = 0.9,
    header = c("Component", paste0(outcome_measure, " (95% CrI)")),
    digits = digits
  )
}

binary_forest_plot <- function(
  obj,
  ...
) {

  args <- list(...)

  components <- obj$components
  stan_summary <- as.data.frame(rstan::summary(obj$fit, pars = c("d"))$summary)
  outcome_measure <- obj$outcome

  if (!hasArg(xlim)) {
    xlim <- c(0, get_xlim(stan_summary, TRUE)[2])
  } else {
    xlim <- args[["xlim"]]
  }

  meta_forest_plot(
    stan_summary,
    components,
    outcome_measure,
    xlim,
    refline = 1,
    transform_function = exp,
    ...
  )
}

continuous_forest_plot <- function(
  obj,
  ...
) {

  args <- list(...)

  components <- obj$components
  stan_summary <- as.data.frame(rstan::summary(obj$fit, pars = c("d"))$summary)
  outcome_measure <- obj$outcome

  if (!hasArg(xlim)) {
    xlim <- get_xlim(stan_summary)
  } else {
    xlim <- args[["xlim"]]
  }

  meta_forest_plot(
    stan_summary,
    components,
    outcome_measure,
    xlim,
    refline = 0,
    ...
  )
}

get_xlim <- function(
  stan_summary,
  binary = FALSE
) {

  lci <- round(min(stan_summary$`2.5%`), 0)
  uci <- round(max(stan_summary$`97.5%`), 0)

  x_min <-  ifelse(binary, exp(lci) - 1, lci - 1)
  x_max <-  ifelse(binary, exp(uci) - 1, uci + 1)

  if (x_min < -5) {
    x_min <- -5
  }

  if (x_max > 10) {
    x_max <- 10
  }

  return(c(x_min, x_max))
}
