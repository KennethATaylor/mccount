#' Calculate Mean Cumulative Count using the equation method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#'
#' @return A tibble with columns for time and MCC (Mean Cumulative Count)
#' @export
mcc_equation <- function(data, id_var, time_var, cause_var) {
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
