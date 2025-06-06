#' Calculate Mean Cumulative Count using the Sum of Cumulative Incidence method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up or event times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#' @param tstart_var Name of the column containing start times of follow-up (as string or symbol, optional).
#'                  If NULL (default), a constant value of 0 is used for all observations.
#' @param adjust_times Whether to automatically adjust times for simultaneous events (default: TRUE)
#' @param time_precision Precision used for adjusting simultaneous events (default: 1e-6)
#' @param include_details Whether to include detailed calculation tables and intermediate
#'                        objects in the output (default: TRUE).
#'
#' @returns A list containing MCC results. If include_details=TRUE, returns complete
#'         calculation details. Otherwise, returns only the final MCC estimates.
#'
#' @keywords internal
mcc_sci <- function(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE
) {
  # Convert inputs to symbols for tidy evaluation
  id_var <- rlang::ensym(id_var)
  time_var <- rlang::ensym(time_var)
  cause_var <- rlang::ensym(cause_var)
  if (!is.null(tstart_var)) {
    tstart_var <- rlang::ensym(tstart_var)
  }

  # Validate data type
  validate_data_type(data)

  # Validate column existence
  validate_column_existence(data, id_var, time_var, cause_var, tstart_var)

  # Validate cause values
  validate_cause_values(data, cause_var)

  # Additional validation for time vs. tstart value pairs
  if (!is.null(tstart_var)) {
    validate_time_tstart(data, time_var, tstart_var)
  }

  # Standardize the data
  data_std <- standardize_data(data, id_var, time_var, cause_var, tstart_var)

  # Validate last row for each `id_var`
  validate_last_observation(data_std)

  # If no events of interest, return default value
  if (sum(data_std$cause == 1) == 0) {
    max_time <- max(data_std$time, na.rm = TRUE)
    cli::cli_warn(
      c(
        "{.arg cause_var} variable includes {.vals {unique(data_std$cause)}} only",
        "i" = "Setting sum of cumulative incidence to {.val {as.numeric(0)}} at maximum time point {.val {max_time}}"
      ),
      wrap = TRUE
    )

    # Create appropriate results based on include_details parameter
    if (include_details) {
      # Detailed output (original behavior)
      result_list <- list(
        mcc_final = tibble::tibble(time = max_time, SumCIs = 0),
        sci_table = tibble::tibble(time = max_time, SumCIs = 0),
        original_data = data_std
      )
    } else {
      # Simplified output for bootstrapping
      result_list <- list(
        mcc_final = tibble::tibble(time = max_time, SumCIs = 0)
      )
    }

    return(result_list)
  }

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

  # Extract vectors for calculation - more efficient for numerical operations
  id <- dt$id
  time <- dt$time
  cause <- dt$cause
  tstart <- dt$tstart

  # Handle tstart as a vector if it's a single value
  if (length(unique(tstart)) == 1) {
    tstart <- rep(tstart[1], length(time))
  }

  # Adjust for identical time and tstart values
  time_equal_tstart <- time == tstart
  if (any(time_equal_tstart)) {
    time[time_equal_tstart] <- tstart[time_equal_tstart] + time_precision
  }

  # Create data.table with all required columns
  dt[, `:=`(
    cause1 = data.table::fcase(cause == 2, -1, default = cause),
    time_adj = time,
    tstart_adj = tstart
  )]

  # Sort once by id, time, and cause priority
  data.table::setorder(dt, id, time_adj, -cause1)

  # Check if we need to account for truncation
  calc_trunc <- any(tstart[!is.na(tstart)] != 0)

  # Add sequence number within each ID
  dt[, first := seq_len(.N), by = id]

  # Maximum number of recurrent events per ID
  recurrent_event_of_interest <- dt[cause == 1, unique(first)]
  max_events <- max(recurrent_event_of_interest)

  # Get the last row for each ID to determine max events per person
  event_number <- dt[, .(maxE = .N), by = id]

  # Merge back to main data
  dt[event_number, maxE := i.maxE, on = "id"]

  # Create dataset for cumulative incidence calculations
  data_mcc_list <- vector("list", max_events)

  # Process first events
  data_mcc_list[[1]] <- dt[first == 1][, m_event := 1]

  # Process subsequent events
  for (i in 2:max_events) {
    # For those with i or more records, take the ith record
    the_ith <- dt[first == i][, m_event := i]

    # For those with <i records, take the last record
    # If the last record is an event 1, change it to 0
    the_last <- dt[maxE < i & first == maxE][,
      `:=`(
        cause = data.table::fcase(cause == 1, 0, default = cause),
        m_event = i
      )
    ]

    data_mcc_list[[i]] <- rbind(the_ith, the_last)
  }

  # Combine all data
  data_mcc <- data.table::rbindlist(data_mcc_list)

  # Store all cumulative incidence results
  all_cis <- vector("list", max_events)

  # Calculate cumulative incidence for each recurrence level
  mcc_base_list <- vector("list", max_events)

  for (j in seq_len(max_events)) {
    data_j <- data_mcc[m_event == j]

    if (1 %in% data_j$cause) {
      # Only if the data contains events of interest
      if (calc_trunc) {
        # For left truncation, use mstate::crprep and survival
        exp_data <- mstate::crprep(
          Tstop = "time_adj",
          status = "cause",
          data = data_j,
          trans = 1,
          cens = 0,
          Tstart = "tstart_adj",
          id = "id"
        )

        fit1 <- survival::survfit(
          survival::Surv(Tstart, Tstop, status == 1) ~ 1,
          data = exp_data,
          weights = exp_data$weight.cens * exp_data$weight.trunc
        )

        cm1 <- data.table::data.table(
          Time = summary(fit1)$time,
          cm = 1 - summary(fit1)$surv
        )
      } else {
        # For right censoring only, use cmprsk::cuminc
        ci_grey <- cmprsk::cuminc(data_j$time_adj, data_j$cause)
        cm1 <- data.table::data.table(
          Time = ci_grey[[1]]$time,
          cm = ci_grey[[1]]$est
        )
      }

      # Store in all_cis list
      all_cis[[j]] <- tibble::tibble(time = cm1$Time, ci = cm1$cm)

      # Calculate incremental changes and add recurrence indicator
      cm1[, `:=`(
        Deta = c(cm[1], diff(cm)),
        cumI = j
      )]

      mcc_base_list[[j]] <- cm1
    } else {
      all_cis[[j]] <- tibble::tibble(time = numeric(0), ci = numeric(0))
    }
  }

  # Combine all mcc_base results
  mcc_base <- data.table::rbindlist(mcc_base_list, fill = TRUE)

  # Process results to create final outputs
  if (nrow(mcc_base) > 0) {
    # Remove duplicates and sort by event dates efficiently
    mcc_base_unique <- unique(mcc_base, by = c("cm", "cumI"))
    data.table::setorder(mcc_base_unique, Time)

    # Calculate MCC by summing cumulative incidences
    mcc_base_unique[, MCC := cumsum(Deta)]

    # Create final output: max MCC value at each time point
    mcc_final_dt <- mcc_base_unique[, .(SumCIs = max(MCC)), by = Time]
    data.table::setnames(mcc_final_dt, "Time", "time")
    mcc_final_dt <- cleanup_output_times(mcc_final_dt, time_precision)

    # Ensure time = 0 row exists
    if (!0 %in% mcc_final_dt$time) {
      time_zero_row <- data.table::data.table(time = 0, SumCIs = 0)
      mcc_final_dt <- data.table::rbindlist(list(time_zero_row, mcc_final_dt))
      data.table::setorder(mcc_final_dt, time)
    }

    mcc_final <- tibble::as_tibble(mcc_final_dt)

    # Create SCItable
    # Get all unique time points from the data used in calculation
    all_times <- sort(unique(time[time > 0]))

    # Create a table with all time points
    sci_table_dt <- data.table::data.table(time = all_times)

    # Fill in CI values for each event number
    for (j in seq_len(max_events)) {
      ci_col <- paste0("CI", j)
      sci_table_dt[, (ci_col) := 0]

      if (length(all_cis[[j]]$time) > 0) {
        ci_data_dt <- data.table::as.data.table(all_cis[[j]])
        ci_data_dt <- ci_data_dt[time > 0]

        if (nrow(ci_data_dt) > 0) {
          # For each time point in sci_table, find the corresponding CI value
          for (i in seq_len(nrow(sci_table_dt))) {
            t <- sci_table_dt$time[i]
            # Find the maximum CI value at or before this time
            ci_before <- ci_data_dt[time <= t, ci]

            if (length(ci_before) > 0) {
              sci_table_dt[i, (ci_col) := max(ci_before)]
            }
          }
        }
      }
    }

    # Calculate SumCIs as the sum of all CI columns
    ci_cols <- paste0("CI", seq_len(max_events))
    existing_ci_cols <- intersect(ci_cols, names(sci_table_dt))

    if (length(existing_ci_cols) > 0) {
      sci_table_dt[, SumCIs := rowSums(.SD), .SDcols = existing_ci_cols]
    } else {
      sci_table_dt[, SumCIs := 0]
    }

    sci_table_dt <- cleanup_output_times(sci_table_dt, time_precision)

    # Add time = 0 row to sci_table if not present
    if (!0 %in% sci_table_dt$time) {
      zero_row_cols <- c("time", existing_ci_cols, "SumCIs")
      zero_row_values <- c(0, rep(0, length(existing_ci_cols)), 0)
      time_zero_row <- data.table::setDT(as.list(stats::setNames(
        zero_row_values,
        zero_row_cols
      )))
      sci_table_dt <- data.table::rbindlist(list(time_zero_row, sci_table_dt))
      data.table::setorder(sci_table_dt, time)
    }

    sci_table <- tibble::as_tibble(sci_table_dt)

    # Rename Time to time in the data.table
    data.table::setnames(mcc_base, "Time", "time")

    mcc_base <- cleanup_output_times(mcc_base, time_precision)

    mcc_base <- tibble::as_tibble(mcc_base[, .(
      time,
      cm,
      Deta,
      cumI
    )])

    # Prepare the return list based on include_details parameter
    if (include_details) {
      result_list <- list(
        mcc_final = mcc_final,
        sci_table = sci_table,
        all_cis = all_cis,
        mcc_base = mcc_base,
        original_data = data_std
      )

      # Only include adjusted_data if times were actually adjusted
      if (times_were_adjusted) {
        result_list$adjusted_data <- adjusted_data
      }
    } else {
      # Simplified output for bootstrapping
      result_list <- list(mcc_final = mcc_final)
    }

    return(result_list)
  } else {
    max_time <- max(time, na.rm = TRUE)
    cli::cli_warn(
      c(
        "No events of interest available for calculation despite events are present in input data.",
        "i" = "This can happen from all events being filtered out after left-truncation or inconsistencies in the input data",
        "i" = "Setting sum of cumulative incidence to {.val {as.numeric(0)}} at maximum time point {.val {max_time}}"
      ),
      wrap = TRUE
    )

    # Create empty results
    # Prepare simplified results if include_details is FALSE
    if (include_details) {
      result_list <- list(
        mcc_final = tibble::tibble(time = max_time, SumCIs = 0),
        sci_table = tibble::tibble(time = max_time, SumCIs = 0),
        original_data = data_std
      )

      # Only include adjusted_data if times were adjusted
      if (times_were_adjusted) {
        result_list$adjusted_data <- adjusted_data
      }
    } else {
      # Simplified output for bootstrapping
      result_list <- list(
        mcc_final = tibble::tibble(time = max_time, SumCIs = 0)
      )
    }

    return(result_list)
  }
}
