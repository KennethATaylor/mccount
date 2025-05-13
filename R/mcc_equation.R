#' Calculate Mean Cumulative Count using the equation method
#'
#' @param id Vector of participant IDs
#' @param time Vector of follow-up times
#' @param cause Vector of event indicators (1=event of interest, 2=competing risk, 0=censoring)
#'
#' @return A tibble with columns for time and MCC (Mean Cumulative Count)
#' @export
mcc_equation <- function(id, time, cause) {
  # Input validation
  if (length(id) != length(time) || length(id) != length(cause)) {
    cli::cli_abort(
      "{.arg id}, {.arg time}, and {.arg cause} must have the same length"
    )
  }

  # Create initial data frame and sort
  data_orig <- tibble::tibble(id = id, time = time, cause = cause)

  # Sort data (if time is the same for 0/1 or 0/2 of the same person, make 0 last)
  data_sorted <- data_orig |>
    dplyr::arrange(id, time, dplyr::desc(cause))

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
      dplyr::arrange(id, time, dplyr::desc(cause))
  }

  # Total number of unique participants
  n_total <- dplyr::n_distinct(id)

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
  results <- lifetable |>
    dplyr::mutate(
      surv_prob = 1 - cmprk / nrisk,
      overall_surv = cumprod(surv_prob),
      overall_surv_prev = dplyr::lag(overall_surv, default = 1),
      mcc_increment = overall_surv_prev * event / nrisk,
      mcc = cumsum(mcc_increment)
    )

  # Return only unique MCC values (first occurrence of each)
  mcc_final <- results |>
    dplyr::group_by(mcc) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(time, mcc)

  return(mcc_final)
}
