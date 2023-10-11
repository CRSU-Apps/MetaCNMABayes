#' data_wide_to_long
#'
#' @description
#' Internal function for converting wide format data to long format
#'
#'
#' @param df data.frame, to be converted from wide to long
#'
#' @return data.frame, converted from wide to long
#'
#' @examples
data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}

#' is_wide
#'
#' @description
#' Internal function to check if a data.frame is in a wide format
#'
#'
#' @param df data.frame, to be checked if in wide format
#'
#' @return logical, true if in wide format
#'
#' @examples
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

get_comps_no_control_component <-
  function(components_column, control_component = NULL) {
    if (is.null(control_component)) {
      control_component <- get_most_frequent_component(components_column)
    }
    tmp_components <- get_unique_components(components_column)
    tmp_components <- tmp_components[!tmp_components == control_component]
    return(tmp_components)
  }

get_single_components <- function(components_column) {
  tmp_components <- components_column
  tmp_components <- tmp_components[!grepl("\\+", tmp_components)]
}

get_n_components <- function(components_column) {
  length(get_unique_components(components_column)) - 1
}

get_most_frequent_component <- function(components_column) {
  components <- get_single_components(components_column)
  component_frequency <- data.frame(table(components))
  most_frequent_component <-
    component_frequency[which.max(component_frequency$Freq), ]$components
  return(most_frequent_component)
}

sort_data <- function(df) {
  require(dplyr)
  tmp_df <- df
  names(tmp_df) <- tolower(names(tmp_df))
  tmp_df <- tmp_df %>%
    dplyr::mutate(study = as.factor(study)) %>% # nolint: object_usage
    dplyr::group_by(study) %>%
    dplyr::mutate(study_id = dplyr::cur_group_id())
  return(tmp_df)
}

format_data <- function(df, data_type, control_component = NULL) {
  df <- sort_data(df)
  n_trials <- get_n_trials(df)
  if (is.null(control_component)) {
    control_component <- get_most_frequent_component(df$components)
  }
  n_arms <- get_n_arms(df)
  n_components <- get_n_components(df$components)
  if (data_type == "binary") {
    return(format_data_binary(
      df,
      n_trials,
      n_arms, n_components,
      control_component
    ))
  } else {
    return(format_data_continous(
      df,
      n_trials,
      n_arms,
      n_components,
      control_component
    ))
  }
}

format_data_binary <- function(
  df,
  n_trials,
  n_arms,
  n_components,
  control_component = NULL
) {
  tmp_df <- sort_data(df)
  r <- get_n_array(tmp_df)
  n <- get_n_array(tmp_df, column = "total")
  components <- get_component_array(df, control_component)
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
  control_component = NULL
) {

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
  require(dplyr)
  tmp_data <- sort_data(df)
  tmp_data <- tmp_data %>%
    dplyr::mutate(n = n()) %>%
    dplyr::select(study, n) %>%
    dplyr::filter(row_number() == 1)
  return(as.numeric(tmp_data$n))
}

get_n_array <- function(df, control_component = NULL, column = "events") {
  require(dplyr)
  tmp_df <- sort_data(df)
  tmp_df <- tmp_df %>%
    dplyr::mutate(study = as.factor(study)) %>% # nolint: object_usage
    dplyr::group_by(study) %>%
    dplyr::mutate(study_id = dplyr::cur_group_id())
  n_trials <- get_n_trials(tmp_df)
  max_arms <- max(get_n_arms(tmp_df))
  n_array <- array(NA, dim = c(n_trials, max_arms))
  if (is.null(control_component)) {
    control_component <- get_most_frequent_component(tmp_df$components)
  }
  for (study_id in 1:max(tmp_df$study_id)){
    study <- tmp_df[tmp_df$study_id == study_id, ]
    study_control_component <- study[study$components == control_component, ]
    study_arms <- study[!study$components == control_component, ]
    # Set control_component
    n_array[study_id, 1] <- study_control_component[[column]]
    for (arm in seq_len(nrow(study_arms))) {
      n_array[study_id, (arm + 1)] <- study_arms[arm, ][[column]]
    }
  }
  return(n_array)
}

get_component_array <- function(df, control_component = NULL) {
  tmp_df <- sort_data(df)
  n_components <- get_n_components(tmp_df$components)
  n_trials <- get_n_trials(tmp_df)
  max_arms <- max(get_n_arms(tmp_df))
  if (is.null(control_component)) {
    control_component <- get_most_frequent_component(tmp_df$components)
  }
  components <- get_comps_no_control_component(tmp_df$components)
  component_array <- array(NA, dim = c(n_components, n_trials, max_arms))
  for(component in seq_len(n_components)){
    tmp_component <- components[component]
    print(tmp_component)
    for(study_id in seq_len(max(tmp_df$study_id))) {
      component_array[component, study_id, 1] <- 0
      tmp_study <- tmp_df[tmp_df$study_id == study_id, ]
      tmp_study <- tmp_study[!tmp_study$components == control_component, ]
      for (arm in seq_len(nrow(tmp_study))) {
        tmp_study_components <- get_all_components(tmp_study[arm, ]$components)
        if (tmp_component %in% tmp_study_components) {
          component_array[component, study_id, (arm + 1)] <- 1
        } else {
          component_array[component, study_id, (arm + 1)] <- 0
        }
      }
    }
  }
  return(component_array)
}
