#' Title
#'
#' @param data a dataframe containing the required columns see details.
#' @param control_component (optional) a character specifying
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
#' fit_binary_fit(df)
#' }
fit_binary_fe <- function(
  data,
  control_component = NULL,
  outcome = "OR",
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
  if (! outcome == "OR" | outcome == "RR") {
    stop("Outcome must be OR or RR")
  }
  df <- data
  if (is_wide(data)) {
    df <- data_wide_to_long(data)
  }
  validate_columns(df, "binary")

  rstan::rstan_options(auto_write = TRUE) # Cache compiled Stan programs
  options(mc.cores = parallel::detectCores()) # Parallelize chains

  components <- get_comps_no_control_component(sort_data(df)$components)

  stan_data <- format_data(df, "binary", control_component)
  stan_fit <- rstan::sampling(
    stanmodels$binary_fe, # nolint: object_usage
    data = stan_data,
    chains = chains,
    warmup = warmup,
    iter = iter,
    seed = seed,
    control = list(
      max_treedepth = max_treedepth,
      adapt_delta = adapt_delta,
      stepsize = stepsize
    )
  )
  gc()
  rtn <- list(
    fit = stan_fit,
    data = stan_data,
    data_type = "binary",
    random_effects = FALSE,
    outcome = outcome,
    components = components
  )
  class(rtn) <- "MetaCNMABayes"
  return(rtn)
}
