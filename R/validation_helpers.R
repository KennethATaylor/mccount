#' Validate existence of required columns
#'
#' @param data The input data frame
#' @param id_var Symbol for ID variable
#' @param time_var Symbol for time variable
#' @param cause_var Symbol for cause variable
#' @param tstart_var Optional symbol for start time variable
#'
#' @return TRUE if all required columns exist (errors otherwise)
#' @keywords internal
#' @noRd
validate_column_existence <- function(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL
) {
  # Convert inputs to names for validation
  required_vars <- c(
    rlang::as_name(id_var),
    rlang::as_name(time_var),
    rlang::as_name(cause_var)
  )

  if (!all(required_vars %in% names(data))) {
    missing_vars <- setdiff(required_vars, names(data))
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

  return(TRUE)
}

#' Validate data type
#'
#' @param data The input data frame
#'
#' @return TRUE if data is a data.frame (errors otherwise)
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
#' @return TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_cause_values <- function(data, cause_var) {
  cause_values <- unique(data[[rlang::as_name(cause_var)]])
  invalid_values <- setdiff(cause_values, c(0, 1, 2))

  if (length(invalid_values) > 0) {
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
#' @return TRUE if validation passes (errors otherwise)
#' @keywords internal
#' @noRd
validate_time_tstart <- function(data, time_var, tstart_var) {
  time_name <- rlang::as_name(time_var)
  tstart_name <- rlang::as_name(tstart_var)

  times <- data[[time_name]]
  tstarts <- data[[tstart_name]]

  if (any(times <= tstarts, na.rm = TRUE)) {
    problematic_indices <- which(times <= tstarts)
    sample_issues <- utils::head(problematic_indices, 5)

    cli::cli_abort(c(
      "Found {length(problematic_indices)} case{?s} where event time is not greater than start time.",
      "i" = "First indices with issues: {sample_issues}",
      "i" = "Ensure all event times are strictly greater than start times."
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
#'
#' @return A standardized data frame with consistent column names
#' @keywords internal
#' @noRd
standardize_data <- function(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL
) {
  # Create standardized data frame
  data_std <- data |>
    dplyr::select(
      id = !!id_var,
      time = !!time_var,
      cause = !!cause_var
    )

  # Add tstart column if specified
  if (!is.null(tstart_var)) {
    data_std <- data_std |>
      dplyr::mutate(tstart = data[[rlang::as_name(tstart_var)]]) |>
      dplyr::relocate("tstart", .before = "time")
  } else {
    data_std <- data_std |>
      dplyr::mutate(tstart = 0) |>
      dplyr::relocate("tstart", .before = "time")
  }

  return(data_std)
}

#' Check for and handle simultaneous events
#'
#' @param data_std Standardized data frame
#' @param adjust_times Whether to adjust times for simultaneous events
#' @param time_precision Precision for time adjustment
#'
#' @return A list with adjusted data (if applicable) and a flag indicating if adjustments were made
#' @keywords internal
#' @noRd
handle_simultaneous_events <- function(data_std, adjust_times, time_precision) {
  # Convert to data.table for efficient operations
  dt <- data.table::as.data.table(data_std)
  data.table::setorder(dt, id, time)

  # Identify simultaneous events efficiently
  dt[, n_events_at_time := .N, by = .(id, time)]
  has_simultaneous_events <- dt[n_events_at_time > 1, .N] > 0

  if (!has_simultaneous_events) {
    return(list(
      data = data_std,
      times_were_adjusted = FALSE
    ))
  }

  if (adjust_times) {
    # Create cause priority ordering: event=1 first, then competing risk=2, then censoring=0
    dt[,
      cause_priority := data.table::fcase(
        cause == 1L,
        1L,
        cause == 2L,
        2L,
        cause == 0L,
        3L,
        default = 4L # fallback for unexpected values
      )
    ]

    # Sort by id, time, then by cause priority
    data.table::setorder(dt, id, time, cause_priority)

    # Add row number within each (id, time) group to determine adjustment order
    dt[, row_within_time := seq_len(.N), by = .(id, time)]

    # Apply time adjustments only where needed (row_within_time > 1)
    dt[
      row_within_time > 1L,
      time := time + time_precision * (row_within_time - 1L)
    ]

    # Clean up helper columns
    dt[, c("n_events_at_time", "cause_priority", "row_within_time") := NULL]

    # Re-sort by final adjusted times
    data.table::setorder(dt, id, time)

    cli::cli_alert_info(
      "Adjusted time points for events occurring simultaneously for the same subject."
    )

    return(list(
      data = tibble::as_tibble(dt),
      times_were_adjusted = TRUE
    ))
  } else {
    # Issue warning but don't adjust
    cli::cli_warn(c(
      "Data contains events occurring simultaneously for the same subject.",
      "i" = "These events will be processed without time adjustment ({.arg adjust_times = FALSE}).",
      "i" = "This may affect calculation accuracy. Consider using {.code adjust_times = TRUE}."
    ))

    return(list(
      data = data_std,
      times_were_adjusted = FALSE
    ))
  }
}
