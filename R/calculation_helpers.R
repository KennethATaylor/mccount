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
