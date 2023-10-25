#' Title
#'
#' @param data a dataframe containing the required columns see details.
#' @param reference_component a character specifying
#' the control component
#' @param outcome (optional) a character specifying the outcome measure.
#' The default is \code{"OR"}
#' @param chains (optional) number of Markov chains.
#' The Default is 3.
#' @param warmup The number of burn-in iterations per chain.
#' The Default is 500
#' @param iter The Number of iterations per chain (including warmup)
#' The default is 1500
#' @param seed The seed for random number generation.
#' The Default is 12345
#' @param max_treedepth (Advanced) See details
#' @param stepsize (Advanced) See details
#'
#' @return A fitted stan model of class MetaCNMABayes
#' @export
#'
#' @examples
#' \dontrun{
#' fit_continuous_fe(df)
#' }
fit_continuous_fe <- function(
  data,
  reference_component,
  outcome = "MD",
  chains = 3,
  warmup = 500,
  iter = 1500,
  seed = 12345,
  max_treedepth = 10,
  adapt_delta = 0.95,
  stepsize = 0.01
) {
  if (! "study" %in% tolower(names(data))) {
    stop("The column study is missing from the data")
  }
  if (! outcome == "MD") {
    stop("Only mean difference is currently supported")
  }
  if (missing(reference_component)) {
    stop("Reference Component cannot be missing")
  }
  return(
    fit_model(
      data,
      reference_component,
      outcome,
      model = stanmodels$continuous_fe,
      data_type = "continuous",
      chains = 3,
      warmup = 500,
      iter = 1500,
      seed = 12345,
      max_treedepth = 10,
      adapt_delta = 0.95,
      stepsize = 0.01
    )
  )
}
