#' Calculate Mean Cumulative Count using the equation method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#' @param time_precision Precision used for adjusting simultaneous events (default: 1e-6)
#' @param adjust_times Whether to automatically adjust times for simultaneous events (default: TRUE)
#'
#' @return A list containing:
#'   \item{mcc_final}{A tibble with columns for time and MCC}
#'   \item{mcc_table}{A tibble with the detailed calculation table}
#'   \item{adjusted_data}{A tibble with the adjusted data (if time adjustment was applied)}
#' @export
mcc_equation <- function(
  data,
  id_var,
  time_var,
  cause_var,
  time_precision = 1e-6,
  adjust_times = TRUE
) {
  # Convert inputs to symbols for tidy evaluation
  id_var <- rlang::ensym(id_var)
  time_var <- rlang::ensym(time_var)
  cause_var <- rlang::ensym(cause_var)

  # Input validation for data type
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble")
  }

  # Input validation for column existence
  if (
    !all(
      c(
        rlang::as_name(id_var),
        rlang::as_name(time_var),
        rlang::as_name(cause_var)
      ) %in%
        names(data)
    )
  ) {
    cli::cli_abort("All specified variables must be columns in {.arg data}")
  }

  # Validate that cause_var only contains values 0, 1, or 2
  cause_values <- unique(data[[rlang::as_name(cause_var)]])
  invalid_values <- setdiff(cause_values, c(0, 1, 2))

  if (length(invalid_values) > 0) {
    cli::cli_abort(c(
      "{.arg cause_var} must only contain values 0, 1, or 2",
      "x" = "Found invalid values: {invalid_values}"
    ))
  }

  # Create initial data frame with consistent column names
  data_orig <- data |>
    dplyr::select(
      id = !!id_var,
      time = !!time_var,
      cause = !!cause_var
    )

  adjusted_data <- data_orig

  # Flag to track if any adjustments were actually made
  times_were_adjusted <- FALSE

  # Identify cases where a subject has different events at the same time
  duplicated_times <- adjusted_data |>
    dplyr::group_by(id, time) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  # Check if there are simultaneous events that need handling
  has_simultaneous_events <- nrow(duplicated_times) > 0

  # Handle simultaneous events based on adjust_times parameter
  if (has_simultaneous_events) {
    if (adjust_times) {
      # Get unique IDs with duplicated times
      affected_ids <- unique(duplicated_times$id)

      for (curr_id in affected_ids) {
        # Extract data for this ID
        id_data <- adjusted_data |>
          dplyr::filter(id == curr_id) |>
          dplyr::arrange(time)

        # Find duplicated times for this ID
        dup_times <- id_data |>
          dplyr::group_by(time) |>
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::ungroup() |>
          dplyr::pull(time) |>
          unique()

        for (dup_time in dup_times) {
          # Process each duplicated time
          dup_data <- id_data |>
            dplyr::filter(time == dup_time)

          # Sort by priority: event=1 first, then competing risk=2, then censoring=0
          dup_data <- dup_data |>
            dplyr::mutate(cause_order = factor(cause, levels = c(1, 2, 0))) |>
            dplyr::arrange(cause_order) |>
            dplyr::select(-cause_order)

          # Adjust times for all but the first event
          if (nrow(dup_data) > 1) {
            for (i in 2:nrow(dup_data)) {
              dup_data$time[i] <- dup_data$time[i] + time_precision * (i - 1)
            }
          }

          # Update the adjusted data
          adjusted_data <- adjusted_data |>
            dplyr::anti_join(
              dplyr::filter(adjusted_data, id == curr_id, time == dup_time),
              by = c("id", "time", "cause")
            ) |>
            dplyr::bind_rows(dup_data) |>
            dplyr::arrange(id, time)
        }
      }

      # Set flag indicating adjustments were made
      times_were_adjusted <- TRUE

      # Inform the user about the adjustment
      cli::cli_alert_info(
        "Adjusted time points for events occurring simultaneously for the same subject."
      )
    } else {
      # If adjust_times=FALSE but simultaneous events exist, issue a warning
      cli::cli_warn(c(
        "Data contains events occurring simultaneously for the same subject.",
        "i" = "These events will be processed without time adjustment ({.arg adjust_times = FALSE}).",
        "i" = "This may affect calculation accuracy. Consider using {.code adjust_times = TRUE}."
      ))
    }
  }

  # Use adjusted_data if adjustments were made, otherwise use original
  data_to_use <- if (times_were_adjusted) adjusted_data else data_orig

  # Sort data
  data_sorted <- data_to_use |>
    dplyr::arrange(id, time)

  # Identify last record for each ID
  last_records <- data_sorted |>
    dplyr::group_by(id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::filter(cause == 1)

  # If any last record is an event, add a censoring record
  if (nrow(last_records) > 0) {
    last_records$cause <- 0
    data_sorted <- dplyr::bind_rows(data_sorted, last_records) |>
      dplyr::arrange(id, time)
  }

  # Total number of unique participants
  n_total <- dplyr::n_distinct(data_sorted$id)

  # Count events by time and cause
  freq_cause <- data_sorted |>
    dplyr::mutate(count = 1) |>
    dplyr::group_by(time, cause) |>
    dplyr::summarize(count = sum(count), .groups = "drop")

  # Create life table format
  lifetable <- freq_cause |>
    tidyr::pivot_wider(
      names_from = cause,
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::rename_with(
      ~ dplyr::case_when(
        . == "1" ~ "event",
        . == "0" ~ "censor",
        . == "2" ~ "cmprk",
        TRUE ~ .
      )
    )

  # Check for missing cause columns and add them if needed
  if (!"censor" %in% names(lifetable)) lifetable$censor <- 0
  if (!"event" %in% names(lifetable)) lifetable$event <- 0
  if (!"cmprk" %in% names(lifetable)) lifetable$cmprk <- 0

  # Calculate cumulative sums for censoring and competing risks
  lifetable <- lifetable |>
    dplyr::mutate(
      sum_censor = cumsum(censor),
      sum_cmprk = cumsum(cmprk)
    ) |>
    dplyr::mutate(
      nrisk_current = n_total - (sum_censor + sum_cmprk),
      nrisk = dplyr::lag(nrisk_current, default = n_total)
    )

  # Calculate survival probabilities and MCC
  mcc_table <- lifetable |>
    dplyr::mutate(
      surv_prob = 1 - cmprk / nrisk,
      overall_surv = cumprod(surv_prob),
      overall_surv_previous = dplyr::lag(overall_surv, default = 1),
      ave_events = overall_surv_previous * event / nrisk,
      mcc = cumsum(ave_events)
    ) |>
    dplyr::select(
      time,
      nrisk,
      censor,
      event,
      cmprk,
      overall_surv_previous,
      ave_events,
      mcc
    )

  # Return only unique MCC values (first occurrence of each)
  mcc_final <- mcc_table |>
    dplyr::group_by(mcc) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(time, mcc)

  # Prepare the return list with conditional inclusion of adjusted_data
  result_list <- list(
    mcc_final = mcc_final,
    mcc_table = mcc_table,
    original_data = data_orig
  )

  # Only include adjusted_data if times were actually adjusted
  if (times_were_adjusted) {
    result_list$adjusted_data <- adjusted_data
  }

  return(result_list)
}
