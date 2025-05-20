#' Calculate Mean Cumulative Count using the equation method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#' @param adjust_times Whether to automatically adjust times for simultaneous events (default: TRUE)
#' @param time_precision Precision used for adjusting simultaneous events (default: 1e-6)
#'
#' @return A list containing:
#'   \item{mcc_final}{A tibble with columns for time and MCC}
#'   \item{mcc_table}{A tibble with the detailed calculation table}
#'   \item{adjusted_data}{A tibble with the adjusted data (if time adjustment was applied)}
#' @keywords internal
mcc_equation <- function(
  data,
  id_var,
  time_var,
  cause_var,
  adjust_times = TRUE,
  time_precision = 1e-6
) {
  # Convert inputs to symbols for tidy evaluation
  id_var <- rlang::ensym(id_var)
  time_var <- rlang::ensym(time_var)
  cause_var <- rlang::ensym(cause_var)

  # Validate data type
  validate_data_type(data)

  # Validate column existence
  validate_column_existence(data, id_var, time_var, cause_var)

  # Validate cause_var values
  validate_cause_values(data, cause_var)

  # Standardize the data
  data_std <- standardize_data(data, id_var, time_var, cause_var)

  # Handle simultaneous events
  sim_events_result <- handle_simultaneous_events(
    data_std,
    adjust_times,
    time_precision
  )
  adjusted_data <- sim_events_result$data
  times_were_adjusted <- sim_events_result$times_were_adjusted

  # Use adjusted_data if adjustments were made, otherwise use original
  data_to_use <- if (times_were_adjusted) adjusted_data else data_std

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
    original_data = data_std
  )

  # Only include adjusted_data if times were actually adjusted
  if (times_were_adjusted) {
    result_list$adjusted_data <- adjusted_data
  }

  return(result_list)
}
