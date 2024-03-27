# Internal function to convert wide format data to long
data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}

# Internal function to determine if data is in a wide format
is_wide <- function(df) {
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}

# Internal function to return all components from all arms
# in a single vector
get_all_components <- function(components_column) {
  components <- levels(as.factor(components_column))
  components <- paste(components, collapse = "+")
  components <- strsplit(components, "\\+")[[1]]
  components <- trimws(components)
  # Convert to factor (for speed)
  components <- as.factor(components)
  return(components)
}

# Internal function to return only unique components
# in a single vector
get_unique_components <- function(components_column) {
  components <- get_all_components(components_column)
  return(
    # Use levels to get unique components
    components <- levels(components)
  )
}

# Internal function to return all unique components
# without the referenct component
get_components_no_ref <-
  function(components_column, reference_component) {
    tmp_components <- get_unique_components(components_column)
    tmp_components <- tmp_components[!tmp_components == reference_component]
    return(tmp_components)
  }

# Internal function to return components in arms with only
# one component
get_single_components <- function(components_column) {
  tmp_components <- components_column
  tmp_components <- tmp_components[!grepl("\\+", tmp_components)]
}

# Internal function to check if the reference component
# is the only component in it's arm
is_reference_single <- function(df, reference_component) {
  tmp_df <- sort_data(df, reference_component)
  single_components <- get_single_components(tmp_df$components)
  n_reference_components <- 0
  min_reference_components <- get_n_trials(tmp_df)
  for (component in single_components) {
    if (component == reference_component) {
      n_reference_components <- n_reference_components + 1
    }
  }
  if (n_reference_components == min_reference_components) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Internal function to return the number of
# unique components excluding the reference
get_n_components <- function(components_column) {
  length(get_unique_components(components_column)) - 1
}

# Function to reorder data
reorder_data <- function(df, reference_component) {
  names(df) <- tolower(names(df))
  tmp_df_ref <- data.frame()
  tmp_df_no_ref <- data.frame()

  for (row_i in seq_len(nrow(df))) {
    tmp_row <- df[row_i,]
    row_components <- get_all_components(tmp_row$components)
    if (
      reference_component %in% row_components && length(row_components) == 1
    ) {
      tmp_df_ref <- rbind(tmp_df_ref, tmp_row)
    } else {
      tmp_df_no_ref <- rbind(tmp_df_no_ref, tmp_row)
    }
  }

  tmp_df <- rbind(tmp_df_ref, tmp_df_no_ref)
  return(tmp_df)
}

# Internal function to group and sort the data
sort_data <- function(df, reference_component) {
  `%>%` <- magrittr::`%>%`
  if (!missing(reference_component)) {
    df <- reorder_data(df, reference_component)
  }
  tmp_df <- df
  names(tmp_df) <- tolower(names(tmp_df))
  tmp_df <- tmp_df %>%
    dplyr::mutate(study = as.factor(study)) %>% # nolint: object_usage
    dplyr::group_by(study) %>%
    dplyr::mutate(study_id = dplyr::cur_group_id()) %>%
    dplyr::mutate(arm = dplyr::row_number()) %>%
    dplyr::arrange(study_id, arm)
  return(tmp_df)
}

# Internal function to return the data
# formatted for STAN
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
    return(format_data_continuous(
      df,
      n_trials,
      n_arms,
      n_components,
      reference_component
    ))
  }
}

# Internal function to return binary data
# formatted for STAN
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

# Internal function to return continuous data
# formatted for STAN
format_data_continuous <- function(
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

# Internal function to return the number of trials
get_n_trials <- function(df, reference_component) {
  tmp_df <- sort_data(df, reference_component)
  return(length(levels(as.factor(tmp_df$study))))
}

# Internal function to get a vecotor of the study names
get_studies <- function(df, reference_component) {
  tmp_df <- sort_data(df, reference_component)
  return(levels(tmp_df$study))
}

# Internal function to get the number of arms per study
get_n_arms <- function(df, reference_component) {
  `%>%` <- magrittr::`%>%`
  tmp_data <- sort_data(df, reference_component)
  tmp_data <- tmp_data %>%
    dplyr::select(study, arm) %>%
    dplyr::filter(arm == max(arm))
  return(as.numeric(tmp_data$arm))
}

# Internal function to get an array of formatted
# data for a given column to be passed to STAN
get_n_array <- function(df, reference_component, column = "events") {
  tmp_df <- sort_data(df, reference_component)
  n_trials <- get_n_trials(tmp_df, reference_component)
  max_arms <- max(get_n_arms(tmp_df, reference_component))
  n_array <- array(1, dim = c(n_trials, max_arms))
  for (study_id in 1:max(tmp_df$study_id)){
    study <- tmp_df[tmp_df$study_id == study_id, ]
    for (arm in seq_len(nrow(study))) {
      n_array[study_id, arm] <- study[arm, ][[column]]
    }
  }
  return(n_array)
}

# Internal function to get an array of formatted
# data for components in a study to be passed to STAN
get_component_array <- function(df, reference_component) {
  tmp_df <- sort_data(df, reference_component)
  n_components <- get_n_components(tmp_df$components)
  n_trials <- get_n_trials(tmp_df, reference_component)
  max_arms <- max(get_n_arms(tmp_df, reference_component))
  components <- get_components_no_ref(tmp_df$components, reference_component)
  component_array <- array(0, dim = c(n_components, n_trials, max_arms))
  for (component in seq_len(n_components)){
    tmp_component <- components[component]
    for (study_id in seq_len(max(tmp_df$study_id))) {
      tmp_study <- tmp_df[tmp_df$study_id == study_id, ]
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
