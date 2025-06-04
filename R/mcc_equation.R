#' Calculate Mean Cumulative Count using the equation method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#' @param weights Name of the column containing weights (as string, optional)
#' @param adjust_times Whether to automatically adjust times for simultaneous events (default: TRUE)
#' @param time_precision Precision used for adjusting simultaneous events (default: 1e-6)
#' @param include_details Whether to include detailed calculation tables and intermediate
#'                        objects in the output (default: TRUE).
#'
#' @returns A list containing MCC results. If `include_details = TRUE`, returns complete
#'         calculation details. Otherwise, returns only the final MCC estimates.
#'
#' @keywords internal
mcc_equation <- function(
  data,
  id_var,
  time_var,
  cause_var,
  weights = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE
) {
  # Convert inputs to symbols for tidy evaluation
  id_var <- rlang::ensym(id_var)
  time_var <- rlang::ensym(time_var)
  cause_var <- rlang::ensym(cause_var)

  # Validate data type
  validate_data_type(data)

  # Validate column existence
  validate_column_existence(
    data,
    id_var,
    time_var,
    cause_var,
    weights = weights
  )

  # Validate cause_var values
  validate_cause_values(data, cause_var)

  # Validate weights if provided
  if (!is.null(weights)) {
    validate_weights_variable(data, weights)
  }

  # Standardize the data
  data_std <- standardize_data(
    data,
    id_var,
    time_var,
    cause_var,
    weights = weights
  )

  # Validate last row for each `id_var`
  validate_last_observation(data_std)

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

  # Convert to data.table for efficient operations
  dt <- data.table::as.data.table(data_to_use)
  data.table::setorder(dt, id, time)

  # Identify last record for each ID efficiently
  last_records <- dt[, .SD[.N], by = id][cause == 1]

  # If any last record is an event, add a censoring record
  if (nrow(last_records) > 0) {
    last_records[, cause := 0]
    dt <- data.table::rbindlist(list(dt, last_records))
    data.table::setorder(dt, id, time)
  }

  # Check if we're doing weighted analysis
  is_weighted <- !is.null(weights) && "weights" %in% names(dt)

  if (is_weighted) {
    # Weighted MCC calculation
    result <- calculate_weighted_mcc(dt, include_details)
  } else {
    # Unweighted MCC calculation (existing logic)
    result <- calculate_unweighted_mcc(dt, include_details)
  }

  # Add original data and adjusted data to result
  if (include_details) {
    result$original_data <- data_std
    if (times_were_adjusted) {
      result$adjusted_data <- adjusted_data
    }
  }

  return(result)
}

#' Calculate unweighted MCC (existing logic)
#'
#' @param dt data.table with standardized data
#' @param include_details Whether to include detailed results
#'
#' @returns List with MCC results
#' @keywords internal
#' @noRd
calculate_unweighted_mcc <- function(dt, include_details) {
  # Total number of unique participants
  n_total <- dt[, data.table::uniqueN(id)]

  # Count events by time and cause efficiently
  freq_cause <- dt[, .(count = .N), by = .(time, cause)]

  # Create life table format efficiently using data.table
  lifetable_dt <- data.table::dcast(
    freq_cause,
    time ~ cause,
    value.var = "count",
    fill = 0
  )

  # Rename columns efficiently
  old_names <- names(lifetable_dt)
  new_names <- old_names
  new_names[new_names == "0"] <- "censor"
  new_names[new_names == "1"] <- "event"
  new_names[new_names == "2"] <- "cmprk"
  data.table::setnames(lifetable_dt, old_names, new_names)

  # Add missing columns if needed and ensure numeric type
  required_cols <- c("censor", "event", "cmprk")
  missing_cols <- setdiff(required_cols, names(lifetable_dt))
  if (length(missing_cols) > 0) {
    lifetable_dt[, (missing_cols) := 0.0] # Use 0.0 to ensure numeric type
  }

  # Ensure all count columns are numeric (not integer)
  count_cols <- c("censor", "event", "cmprk")
  existing_count_cols <- intersect(count_cols, names(lifetable_dt))
  lifetable_dt[,
    (existing_count_cols) := lapply(.SD, as.numeric),
    .SDcols = existing_count_cols
  ]

  # Calculate cumulative sums and other measures efficiently
  lifetable_dt[, `:=`(
    sum_censor = cumsum(censor),
    sum_cmprk = cumsum(cmprk)
  )]

  lifetable_dt[, `:=`(
    nrisk_current = n_total - (sum_censor + sum_cmprk),
    nrisk = data.table::shift(
      n_total - (sum_censor + sum_cmprk),
      n = 1,
      fill = n_total
    )
  )]

  # Calculate survival probabilities and MCC efficiently
  lifetable_dt[, `:=`(
    surv_prob = 1 - cmprk / nrisk,
    overall_surv = cumprod(1 - cmprk / nrisk)
  )]

  lifetable_dt[,
    overall_surv_previous := data.table::shift(overall_surv, n = 1, fill = 1)
  ]
  lifetable_dt[, ave_events := overall_surv_previous * event / nrisk]
  lifetable_dt[, mcc := cumsum(ave_events)]

  # Select required columns
  mcc_table_dt <- lifetable_dt[, .(
    time,
    nrisk,
    censor,
    event,
    cmprk,
    overall_surv_previous,
    ave_events,
    mcc
  )]

  # Return only unique MCC values (first occurrence of each) efficiently
  mcc_final_dt <- mcc_table_dt[, .SD[1], by = mcc][, .(time, mcc)]

  # Convert to tibbles for output and clean up data.table attributes
  mcc_final <- tibble::as_tibble(mcc_final_dt)
  mcc_table <- tibble::as_tibble(mcc_table_dt)

  # Remove data.table attributes to ensure clean tibbles
  attr(mcc_final, ".internal.selfref") <- NULL
  attr(mcc_table, ".internal.selfref") <- NULL
  attr(mcc_table, "sorted") <- NULL

  # Prepare the return list based on include_details parameter
  if (include_details) {
    result_list <- list(
      mcc_final = mcc_final,
      mcc_table = mcc_table
    )
  } else {
    # Simplified output for bootstrapping
    result_list <- list(
      mcc_final = mcc_final
    )
  }

  return(result_list)
}

#' Calculate weighted MCC following Gaber et al. (2023)
#'
#' @param dt data.table with standardized data including weights column
#' @param include_details Whether to include detailed results
#'
#' @returns List with weighted MCC results
#' @keywords internal
#' @noRd
calculate_weighted_mcc <- function(dt, include_details) {
  # Each person should contribute their weight once, not once per record
  # Take the first weight for each ID (assuming weights are constant within ID)
  n_total_weighted <- dt[, .(weight_per_person = weights[1]), by = id][, sum(
    weight_per_person
  )]
  # Calculate weighted event counts by time and cause
  freq_cause_weighted <- dt[, .(count = sum(weights)), by = .(time, cause)]

  # Create life table format efficiently using data.table
  lifetable_dt <- data.table::dcast(
    freq_cause_weighted,
    time ~ cause,
    value.var = "count",
    fill = 0
  )

  # Rename columns efficiently
  old_names <- names(lifetable_dt)
  new_names <- old_names
  new_names[new_names == "0"] <- "censor"
  new_names[new_names == "1"] <- "event"
  new_names[new_names == "2"] <- "cmprk"
  data.table::setnames(lifetable_dt, old_names, new_names)

  # Add missing columns if needed and ensure numeric type
  required_cols <- c("censor", "event", "cmprk")
  missing_cols <- setdiff(required_cols, names(lifetable_dt))
  if (length(missing_cols) > 0) {
    lifetable_dt[, (missing_cols) := 0.0] # Use 0.0 to ensure numeric type
  }

  # Ensure all count columns are numeric
  count_cols <- c("censor", "event", "cmprk")
  existing_count_cols <- intersect(count_cols, names(lifetable_dt))
  lifetable_dt[,
    (existing_count_cols) := lapply(.SD, as.numeric),
    .SDcols = existing_count_cols
  ]

  # Calculate cumulative sums and weighted measures
  lifetable_dt[, `:=`(
    sum_censor = cumsum(censor),
    sum_cmprk = cumsum(cmprk)
  )]

  lifetable_dt[, `:=`(
    nrisk_current = n_total_weighted - (sum_censor + sum_cmprk),
    nrisk = data.table::shift(
      n_total_weighted - (sum_censor + sum_cmprk),
      n = 1,
      fill = n_total_weighted
    )
  )]

  # Calculate weighted survival probabilities and MCC
  lifetable_dt[, `:=`(
    surv_prob = 1 - cmprk / nrisk,
    overall_surv = cumprod(1 - cmprk / nrisk)
  )]

  lifetable_dt[,
    overall_surv_previous := data.table::shift(overall_surv, n = 1, fill = 1)
  ]
  lifetable_dt[, ave_events := overall_surv_previous * event / nrisk]
  lifetable_dt[, mcc := cumsum(ave_events)]

  # Select required columns
  mcc_table_dt <- lifetable_dt[, .(
    time,
    nrisk,
    censor,
    event,
    cmprk,
    overall_surv_previous,
    ave_events,
    mcc
  )]

  # Return only unique MCC values (first occurrence of each) efficiently
  mcc_final_dt <- mcc_table_dt[, .SD[1], by = mcc][, .(time, mcc)]

  # Convert to tibbles for output and clean up data.table attributes
  mcc_final <- tibble::as_tibble(mcc_final_dt)
  mcc_table <- tibble::as_tibble(mcc_table_dt)

  # Remove data.table attributes to ensure clean tibbles
  attr(mcc_final, ".internal.selfref") <- NULL
  attr(mcc_table, ".internal.selfref") <- NULL
  attr(mcc_table, "sorted") <- NULL

  # Prepare the return list based on include_details parameter
  if (include_details) {
    result_list <- list(
      mcc_final = mcc_final,
      mcc_table = mcc_table
    )
  } else {
    # Simplified output for bootstrapping
    result_list <- list(
      mcc_final = mcc_final
    )
  }

  return(result_list)
}
