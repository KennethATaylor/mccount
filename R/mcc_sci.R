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

  # Extract vectors for calculation - more efficient for numerical operations
  id <- data_to_use$id
  time <- data_to_use$time
  cause <- data_to_use$cause
  tstart <- data_to_use$tstart

  # Handle tstart as a vector if it's a single value
  if (length(unique(tstart)) == 1) {
    tstart <- rep(tstart[1], length(time))
  }

  # Adjust for identical time and tstart values
  time_equal_tstart <- time == tstart
  if (any(time_equal_tstart)) {
    time[time_equal_tstart] <- tstart[time_equal_tstart] + exp(-13)
  }

  # Create and sort dataset
  input_data <- tibble::tibble(
    id = id,
    time = time,
    cause = cause,
    tstart = tstart
  ) |>
    dplyr::mutate(cause1 = dplyr::if_else(cause == 2, -1, cause)) |>
    dplyr::arrange(id, time, dplyr::desc(.data$cause1))

  # Check if we need to account for truncation
  calc_trunc <- any(tstart[!is.na(tstart)] != 0)

  # Add sequence number within each ID
  input_data <- input_data |>
    dplyr::group_by(id) |>
    dplyr::mutate(first = dplyr::row_number()) |>
    dplyr::ungroup()

  # Maximum number of events per ID
  max_events <- max(input_data$first)

  # Get the last row for each ID to determine max events per person
  event_number <- input_data |>
    dplyr::group_by(id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::select("id", "first") |>
    dplyr::rename(maxE = "first") |>
    dplyr::ungroup()

  # Merge back to main data
  all_data <- dplyr::left_join(input_data, event_number, by = "id")

  # Create dataset for cumulative incidence calculations
  data_mcc <- NULL

  # Process first events
  data_temp <- all_data |>
    dplyr::filter(.data$first == 1) |>
    dplyr::mutate(m_event = 1)
  data_mcc <- data_temp

  # Process subsequent events
  for (i in 2:max_events) {
    # For those with i or more records, take the ith record
    the_ith <- all_data |> dplyr::filter(.data$first == i)

    # For those with <i records, take the last record
    # If the last record is an event 1, change it to 0
    the_last <- all_data |>
      dplyr::filter(.data$maxE < i, .data$first == .data$maxE) |>
      dplyr::mutate(cause = ifelse(cause == 1, 0, cause))

    ci_data_ith <- dplyr::bind_rows(the_ith, the_last) |>
      dplyr::mutate(m_event = i)

    data_mcc <- dplyr::bind_rows(data_mcc, ci_data_ith)
  }

  # Store all cumulative incidence results
  all_cis <- list()

  # Calculate cumulative incidence for each recurrence level
  mcc_base <- NULL

  for (j in 1:max_events) {
    data_j <- data_mcc |> dplyr::filter(.data$m_event == j)

    if (1 %in% data_j$cause) {
      # Only if the data contains events of interest
      if (calc_trunc) {
        # For left truncation, use mstate::crprep and survival
        exp_data <- mstate::crprep(
          Tstop = "time",
          status = "cause",
          data = data_j,
          trans = 1,
          cens = 0,
          Tstart = "tstart",
          id = "id"
        )

        fit1 <- survival::survfit(
          survival::Surv(Tstart, Tstop, status == 1) ~ 1,
          data = exp_data,
          weights = exp_data$weight.cens * exp_data$weight.trunc
        )

        cm1 <- tibble::tibble(
          Time = summary(fit1)$time,
          cm = 1 - summary(fit1)$surv
        )
      } else {
        # For right censoring only, use cmprsk::cuminc
        ci_grey <- cmprsk::cuminc(data_j$time, data_j$cause)
        cm1 <- tibble::tibble(
          Time = ci_grey[[1]]$time,
          cm = ci_grey[[1]]$est
        )
      }

      # Store in all_cis list
      all_cis[[j]] <- cm1 |> dplyr::rename(time = "Time", ci = "cm")

      # Calculate incremental changes and add recurrence indicator
      cm1 <- cm1 |>
        dplyr::mutate(
          Deta = c(.data$cm[1], diff(.data$cm)),
          cumI = j
        )

      mcc_base <- dplyr::bind_rows(mcc_base, cm1)
    } else {
      all_cis[[j]] <- tibble::tibble(time = numeric(0), ci = numeric(0))
    }
  }

  # Process results to create final outputs
  if (!is.null(mcc_base)) {
    # Remove duplicates and sort by event dates
    nodup_mcc_base <- mcc_base |>
      dplyr::distinct(.data$cm, .data$cumI, .keep_all = TRUE)

    sort_mcc_base <- nodup_mcc_base |>
      dplyr::arrange(.data$Time)

    # Calculate MCC by summing cumulative incidences
    mcc_values <- cumsum(sort_mcc_base$Deta)

    # Combine results
    combine_mcc <- sort_mcc_base |>
      dplyr::mutate(MCC = mcc_values)

    # Create final output: max MCC value at each time point
    mcc_final <- combine_mcc |>
      dplyr::group_by(.data$Time) |>
      dplyr::summarize(SumCIs = max(.data$MCC), .groups = "drop") |>
      dplyr::rename(time = "Time")

    # Create SCItable
    # Get all unique time points from the data used in calculation
    all_times <- sort(unique(time))
    all_times <- all_times[all_times > 0] # Remove time 0 if present

    # Create a table with all time points
    sci_table <- tibble::tibble(time = all_times)

    # Fill in CI values for each event number
    for (j in 1:max_events) {
      ci_col <- paste0("CI", j)
      sci_table[[ci_col]] <- 0 # Initialize with zeros

      if (length(all_cis[[j]]$time) > 0) {
        ci_data <- all_cis[[j]]
        ci_data <- ci_data |> dplyr::filter(.data$time > 0) # Remove time 0

        if (nrow(ci_data) > 0) {
          # For each time point in sci_table, find the corresponding CI value
          for (i in 1:nrow(sci_table)) {
            t <- sci_table$time[i]
            # Find the maximum CI value at or before this time
            ci_before <- ci_data |>
              dplyr::filter(.data$time <= t) |>
              dplyr::pull(.data$ci)

            if (length(ci_before) > 0) {
              sci_table[[ci_col]][i] <- max(ci_before)
            }
          }
        }
      }
    }

    # Calculate SumCIs as the sum of all CI columns
    ci_cols <- grep("^CI", names(sci_table))
    sci_table <- sci_table |>
      dplyr::mutate(
        SumCIs = rowSums(dplyr::across(dplyr::all_of(names(sci_table)[
          ci_cols
        ])))
      )

    # Prepare the return list based on include_details parameter
    if (include_details) {
      result_list <- list(
        mcc_final = mcc_final,
        sci_table = sci_table,
        all_cis = all_cis,
        mcc_base = mcc_base |> dplyr::rename(time = "Time"),
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
