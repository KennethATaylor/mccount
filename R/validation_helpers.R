#' Validate existence of required columns
#'
#' @param data The input data frame
#' @param id_var Symbol for ID variable
#' @param time_var Symbol for time variable
#' @param cause_var Symbol for cause variable
#' @param tstart_var Optional symbol for start time variable
#' @param weights Optional string for weights variable
#'
#' @returns TRUE if all required columns exist (errors otherwise)
#' @keywords internal
#' @noRd
validate_column_existence <- function(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL,
  weights = NULL
) {
  # Convert inputs to names for validation
  required_vars <- c(
    rlang::as_name(id_var),
    rlang::as_name(time_var),
    rlang::as_name(cause_var)
  )

  # Vectorized check for all required columns at once
  missing_vars <- required_vars[!required_vars %in% names(data)]

  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Missing required variables in {.arg data}:",
      "x" = "Missing: {missing_vars}"
    ))
  }

  # Check tstart_var if provided
  if (!is.null(tstart_var)) {
    tstart_name <- rlang::as_name(tstart_var)
    if (!tstart_name %in% names(data)) {
      cli::cli_abort(
        "{.arg tstart_var} column '{tstart_name}' not found in {.arg data}"
      )
    }
  }

  # Check weights if provided
  if (!is.null(weights)) {
    if (!weights %in% names(data)) {
      cli::cli_abort(
        "{.arg weights} column '{weights}' not found in {.arg data}"
      )
    }
  }

  return(TRUE)
}

#' Validate data type
#'
#' @param data The input data frame
#'
#' @returns TRUE if data is a data.frame (errors otherwise)
#' @keywords internal
#' @noRd
validate_data_type <- function(data) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a {.cls data.frame} or {.cls tbl_df}")
  }
  return(TRUE)
}

#' Validate cause variable values
#'
#' @param data Input data
#' @param cause_var Symbol for cause variable
#'
#' @returns TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_cause_values <- function(data, cause_var) {
  cause_col <- data[[rlang::as_name(cause_var)]]

  # More efficient validation using vectorized operations
  valid_values <- c(0, 1, 2)
  invalid_mask <- !cause_col %in% valid_values

  if (any(invalid_mask)) {
    invalid_values <- unique(cause_col[invalid_mask])
    cli::cli_abort(c(
      "{.arg cause_var} must only contain values 0, 1, or 2",
      "x" = "Found invalid values: {invalid_values}"
    ))
  }

  return(TRUE)
}

#' Validate time and tstart values (for SCI method)
#'
#' @param data Input data
#' @param time_var Symbol for time variable
#' @param tstart_var Symbol for start time variable
#'
#' @returns TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_time_tstart <- function(data, time_var, tstart_var) {
  time_name <- rlang::as_name(time_var)
  tstart_name <- rlang::as_name(tstart_var)

  times <- data[[time_name]]
  tstarts <- data[[tstart_name]]

  # Vectorized check for efficiency
  invalid_mask <- times <= tstarts & !is.na(times) & !is.na(tstarts)

  if (any(invalid_mask)) {
    problematic_indices <- which(invalid_mask)
    sample_issues <- utils::head(problematic_indices, 5)

    cli::cli_abort(c(
      "Found {length(problematic_indices)} case{?s} where event time is not greater than start time.",
      "i" = "First indices with issues: {sample_issues}",
      "i" = "Ensure all event times are strictly greater than start times."
    ))
  }

  return(TRUE)
}

#' Validate by variable
#'
#' @param data Input data
#' @param by_var String name of grouping variable
#'
#' @returns TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_by_variable <- function(data, by_var) {
  if (is.null(by_var)) {
    return(TRUE)
  }

  # Check if by_var is a single character string
  if (!is.character(by_var) || length(by_var) != 1) {
    cli::cli_abort(c(
      "{.arg by} must be a single character string",
      "x" = "Received: {.val {by_var}}"
    ))
  }

  # Check if column exists
  if (!by_var %in% names(data)) {
    cli::cli_abort(c(
      "Column specified in {.arg by} not found in {.arg data}",
      "x" = "Column '{by_var}' does not exist"
    ))
  }

  # Check if there are any non-NA values
  by_values <- data[[by_var]]
  if (all(is.na(by_values))) {
    cli::cli_abort(c(
      "All values in {.arg by} variable are missing (NA)",
      "x" = "Column '{by_var}' contains only NA values"
    ))
  }

  # Check for reasonable number of groups (optional warning)
  unique_groups <- unique(by_values[!is.na(by_values)])
  n_groups <- length(unique_groups)

  if (n_groups > 20) {
    cli::cli_warn(c(
      "Large number of groups detected in {.arg by} variable",
      "i" = "Found {n_groups} unique groups in '{by_var}'",
      "i" = "Consider whether this many groups is intended"
    ))
  }

  return(TRUE)
}

#' Validate weights variable
#'
#' @param data Input data
#' @param weights_var String name of weights variable
#'
#' @returns TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_weights_variable <- function(data, weights_var) {
  if (is.null(weights_var)) {
    return(TRUE)
  }

  # Check if weights_var is a single character string
  if (!is.character(weights_var) || length(weights_var) != 1) {
    cli::cli_abort(c(
      "{.arg weights} must be a single character string",
      "x" = "Received: {.val {weights_var}}"
    ))
  }

  # Check if column exists
  if (!weights_var %in% names(data)) {
    cli::cli_abort(c(
      "Column specified in {.arg weights} not found in {.arg data}",
      "x" = "Column '{weights_var}' does not exist"
    ))
  }

  # Extract weights values
  weights_values <- data[[weights_var]]

  # Check for missing values
  if (any(is.na(weights_values))) {
    n_missing <- sum(is.na(weights_values))
    cli::cli_abort(c(
      "Missing values found in {.arg weights} column",
      "x" = "Found {n_missing} missing value{?s} in column '{weights_var}'",
      "i" = "All weights must be non-missing"
    ))
  }

  # Check for non-numeric values
  if (!is.numeric(weights_values)) {
    cli::cli_abort(c(
      "{.arg weights} column must contain numeric values",
      "x" = "Column '{weights_var}' is of type {.cls {class(weights_values)}}"
    ))
  }

  # Check for negative values
  if (any(weights_values < 0)) {
    n_negative <- sum(weights_values < 0)
    min_weight <- min(weights_values)
    cli::cli_abort(c(
      "Negative values found in {.arg weights} column",
      "x" = "Found {n_negative} negative value{?s} in column '{weights_var}'",
      "x" = "Minimum weight: {.val {min_weight}}",
      "i" = "All weights must be non-negative (>= 0)"
    ))
  }

  # Optional warning for extreme weights
  if (any(weights_values == 0)) {
    n_zero <- sum(weights_values == 0)
    cli::cli_warn(c(
      "Zero weights found in {.arg weights} column",
      "!" = "Found {n_zero} zero weight{?s} in column '{weights_var}'",
      "i" = "Observations with zero weights will not contribute to the analysis"
    ))
  }

  # Check for extremely large weights (optional warning)
  weight_ratio <- max(weights_values) / min(weights_values[weights_values > 0])
  if (weight_ratio > 100) {
    cli::cli_warn(c(
      "Large variation in weights detected",
      "!" = "Maximum to minimum weight ratio: {.val {round5(weight_ratio, 2)}}",
      "i" = "Consider checking for potential outliers in weights"
    ))
  }

  return(TRUE)
}

#' Create standardized dataset for MCC calculations
#'
#' @param data The input data frame
#' @param id_var Symbol for ID variable
#' @param time_var Symbol for time variable
#' @param cause_var Symbol for cause variable
#' @param tstart_var Optional symbol for start time variable
#' @param weights Optional string for weights variable
#'
#' @returns A standardized data frame with consistent column names
#' @keywords internal
#' @noRd
standardize_data <- function(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL,
  weights = NULL
) {
  # Create standardized data frame - more efficient column selection
  if (!is.null(tstart_var)) {
    data_std <- data |>
      dplyr::select(
        id = !!id_var,
        tstart = !!tstart_var,
        time = !!time_var,
        cause = !!cause_var
      )
  } else {
    data_std <- data |>
      dplyr::select(
        id = !!id_var,
        time = !!time_var,
        cause = !!cause_var
      ) |>
      dplyr::mutate(tstart = 0) |>
      dplyr::relocate("tstart", .before = "time")
  }

  # Add weights column if provided
  if (!is.null(weights)) {
    data_std <- data_std |>
      dplyr::mutate(weights = data[[weights]]) |>
      dplyr::relocate("weights", .after = "cause")
  }

  return(data_std)
}

#' Validate last observation for each participant
#'
#' Checks whether the final observation for each unique participant has an appropriate
#' cause value (0 = censoring or 2 = competing risk). Issues a warning if any
#' participant's last observation is an event of interest (cause = 1), as this
#' may impact MCC calculations if the participant was actually censored or
#' experienced a competing risk after the recorded event.
#'
#' @param data_std Standardized data frame with columns: id, tstart, time, cause
#'
#' @returns TRUE if validation passes (with potential warning)
#' @keywords internal
#' @noRd
validate_last_observation <- function(data_std) {
  # Convert to data.table for efficient operations
  dt <- data.table::as.data.table(data_std)

  # Sort by id and time to ensure we get the last observation
  data.table::setorder(dt, id, time)

  # Get the last observation for each ID
  last_obs <- dt[, .SD[.N], by = id]

  # Find IDs where the last observation is an event of interest (cause = 1)
  event_last_ids <- last_obs$cause == 1

  if (any(event_last_ids)) {
    # Get the actual ID values for informative warning
    problematic_ids <- last_obs$id[event_last_ids]
    n_problematic <- length(problematic_ids)

    # Create the ID display string
    if (n_problematic <= 5) {
      id_display <- paste(problematic_ids, collapse = ", ")
      id_info <- "ID{?s}: {id_display}"
    } else {
      sample_ids <- utils::head(problematic_ids, 5)
      id_display <- paste(sample_ids, collapse = ", ")
      id_info <- c(
        "First 5 IDs: {id_display}",
        "Total affected: {n_problematic} participants"
      )
    }

    cli::cli_warn(
      c(
        "Found {n_problematic} participant{?s} where last observation is an event of interest ({.arg cause_var} = 1)",
        "!" = id_info,
        "i" = "{.fn mcc} assumes these participants are censored at their final {.arg time_var}",
        "i" = "If participants were actually censored or experienced competing risks after their last event, add those observations to ensure correct estimates"
      ),
      wrap = TRUE
    )
  }

  return(TRUE)
}
