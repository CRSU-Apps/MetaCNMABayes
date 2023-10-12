fit_binary_fe <- function(
  data,
  control_component = NULL,
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
  df <- data
  if (is_wide(data)) {
    df <- data_wide_to_long(data)
  }
  validate_columns(df, "binary")

  rstan::rstan_options(auto_write = TRUE) # Cache compiled Stan programs
  options(mc.cores = parallel::detectCores()) # Parallelize chains

  stan_data <- format_data(df, "binary", control_component)
  stan_fit <- rstan::stan(
    file = system.file("models", "binary_FE.stan", package = "MetaCNMABayes"),
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
  return(stan_fit)
}
