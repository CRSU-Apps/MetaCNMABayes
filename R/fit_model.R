# Internal function for fitting a given stan model
# with given data based on outcome
fit_model <- function(
  data,
  reference_component,
  outcome,
  model,
  data_type,
  chains = 3,
  warmup = 1000,
  iter = 3000,
  seed = 12345,
  max_treedepth = 10,
  adapt_delta = 0.95,
  stepsize = 0.01
) {
  df <- data
  if (is_wide(data)) {
    df <- data_wide_to_long(data)
  }
  validate_columns(df, data_type)

  if (!is_reference_single(df, reference_component)) {
    stop("Currently, only studies where no other
    component is present in the arm containing the reference
    component is supported.")
  }

  rstan::rstan_options(auto_write = TRUE) # Cache compiled Stan programs
  options(mc.cores = parallel::detectCores()) # Parallelize chains

  components <- get_components_no_ref(
    sort_data(df)$components, reference_component
  )

  stan_data <- format_data(df, data_type, reference_component)
  stan_fit <- rstan::sampling(
    model,
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
    data_type = data_type,
    random_effects = FALSE,
    outcome = outcome,
    components = components
  )
  class(rtn) <- "MetaCNMABayes"
  return(rtn)
}
