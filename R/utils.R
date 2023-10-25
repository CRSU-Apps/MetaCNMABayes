data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}

is_wide <- function(df) {
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}

get_all_components <- function(components_column) {
  components <- levels(as.factor(components_column))
  components <- paste(components, collapse = "+")
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
  return(components)
}

get_unique_components <- function(components_column) {
  components <- get_all_components(components_column)
  return(
    # Use levels to get unique components
    components <- levels(components)
  )
}

get_components_no_ref <-
  function(components_column, reference_component) {
    tmp_components <- get_unique_components(components_column)
    tmp_components <- tmp_components[!tmp_components == reference_component]
    return(tmp_components)
  }

get_single_components <- function(components_column) {
  tmp_components <- components_column
  tmp_components <- tmp_components[!grepl("\\+", tmp_components)]
}

get_n_components <- function(components_column) {
  length(get_unique_components(components_column)) - 1
}

sort_data <- function(df, reference_component) {
  `%>%` <- magrittr::`%>%`
  tmp_df <- df
  names(tmp_df) <- tolower(names(tmp_df))
  if (!missing(reference_component)) {
    tmp_df <- reorder_data(tmp_df, reference_component)
  }
  tmp_df <- tmp_df %>%
    dplyr::mutate(study = as.factor(study)) %>% # nolint: object_usage
    dplyr::group_by(study) %>%
    dplyr::mutate(study_id = dplyr::cur_group_id())
  return(tmp_df)
}

reorder_data <- function(df, reference_component) {
  tmp_df_ref <- data.frame()
  tmp_df_no_ref <- data.frame()

  for (row_i in seq_len(nrow(df))) {
    tmp_row <- df[row_i,]
    row_components <- get_all_components(tmp_row$components)
    if (reference_component %in% row_components) {
      tmp_df_ref <- rbind(tmp_df_ref, tmp_row)
    } else {
      tmp_df_no_ref <- rbind(tmp_df_no_ref, tmp_row)
    }
  }

  tmp_df <- rbind(tmp_df_ref, tmp_df_no_ref)
  return(tmp_df)
}

format_data <- function(df, data_type, reference_component) {
  df <- sort_data(df, reference_component)
  n_trials <- get_n_trials(df)
  n_arms <- get_n_arms(df)
  n_components <- get_n_components(df$components)
  if (data_type == "binary") {
    return(format_data_binary(
      df,
      n_trials,
      n_arms, n_components,
      reference_component
    ))
  } else {
    return(format_data_continous(
      df,
      n_trials,
      n_arms,
      n_components,
      reference_component
    ))
  }
}

format_data_binary <- function(
  df,
  n_trials,
  n_arms,
  n_components,
  reference_component
) {
  tmp_df <- sort_data(df, reference_component)
  r <- get_n_array(tmp_df, reference_component)
  n <- get_n_array(tmp_df, reference_component, column = "total")
  components <- get_component_array(df, reference_component)
  return(
    list(
      n_trials = n_trials,
      n_arms = n_arms,
      n_components = n_components,
      n = n,
      r = r,
      components = components
    )
  )
}

format_data_continous <- function(
  df,
  n_trials,
  n_arms,
  n_components,
  reference_component
) {
  tmp_df <- sort_data(df, reference_component)
  n <- get_n_array(tmp_df, reference_component, column = "total")
  y <- get_n_array(tmp_df, reference_component, column = "mean")
  sd <- get_n_array(tmp_df, reference_component, column = "sd")
  components <- get_component_array(df, reference_component)
  return(
    list(
      n_trials = n_trials,
      n_arms = n_arms,
      n_components = n_components,
      n = n,
      y = y,
      sd = sd,
      components = components
    )
  )
}

get_n_trials <- function(df) {
  tmp_df <- sort_data(df)
  return(length(levels(as.factor(tmp_df$study))))
}

get_studies <- function(df) {
  tmp_df <- sort_data(df)
  return(levels(tmp_df$study))
}

get_n_arms <- function(df) {
  `%>%` <- magrittr::`%>%`
  tmp_data <- sort_data(df)
  tmp_data <- tmp_data %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::select(study, n) %>%
    dplyr::filter(dplyr::row_number() == 1)
  return(as.numeric(tmp_data$n))
}

get_n_array <- function(df, reference_component, column = "events") {
  `%>%` <- magrittr::`%>%`
  tmp_df <- sort_data(df, reference_component)
  tmp_df <- tmp_df %>%
    dplyr::mutate(study = as.factor(study)) %>% # nolint: object_usage
    dplyr::group_by(study) %>%
    dplyr::mutate(study_id = dplyr::cur_group_id())
  n_trials <- get_n_trials(tmp_df)
  max_arms <- max(get_n_arms(tmp_df))
  n_array <- array(NA, dim = c(n_trials, max_arms))
  for (study_id in 1:max(tmp_df$study_id)){
    study <- tmp_df[tmp_df$study_id == study_id, ]
    #study_arms <- study[!study$components == reference_component, ]
    for (arm in seq_len(nrow(study))) {
      n_array[study_id, arm] <- study[arm, ][[column]]
    }
  }
  return(n_array)
}

get_component_array <- function(df, reference_component) {
  tmp_df <- sort_data(df, reference_component)
  n_components <- get_n_components(tmp_df$components)
  n_trials <- get_n_trials(tmp_df)
  max_arms <- max(get_n_arms(tmp_df))
  components <- get_components_no_ref(tmp_df$components, reference_component)
  component_array <- array(NA, dim = c(n_components, n_trials, max_arms))
  for (component in seq_len(n_components)){
    tmp_component <- components[component]
    for (study_id in seq_len(max(tmp_df$study_id))) {
      #component_array[component, study_id, 1] <- 0
      tmp_study <- tmp_df[tmp_df$study_id == study_id, ]
      #tmp_study <- tmp_study[!tmp_study$components == reference_component, ]
      for (arm in seq_len(nrow(tmp_study))) {
        tmp_study_components <- get_all_components(tmp_study[arm, ]$components)
        if (tmp_component %in% tmp_study_components) {
          component_array[component, study_id, (arm)] <- 1
        } else {
          component_array[component, study_id, (arm)] <- 0
        }
      }
    }
  }
  return(component_array)
}
