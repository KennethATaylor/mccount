#' Calculate Mean Cumulative Count using the Sum of Cumulative Incidence method
#'
#' @param data A data.frame or tibble containing the required variables
#' @param id_var Name of the column containing participant IDs (as string or symbol)
#' @param time_var Name of the column containing follow-up or event times (as string or symbol)
#' @param cause_var Name of the column containing event indicators (as string or symbol)
#'                 (1=event of interest, 2=competing risk, 0=censoring)
#' @param tstart_var Name of the column containing start times of follow-up (as string or symbol, optional).
#'                  If NULL (default), a constant value of 0 is used for all observations.
#'
#' @return A tibble with columns for time and MCC (expressed as SumCIs)
#'
#' @export
mcc_sci <- function(data, id_var, time_var, cause_var, tstart_var = NULL) {
  # Convert inputs to symbols for tidy evaluation
  id_var <- rlang::ensym(id_var)
  time_var <- rlang::ensym(time_var)
  cause_var <- rlang::ensym(cause_var)

  # Input validation for data type
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble")
  }

  # Input validation for column existence
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

  # Handle tstart_var - either extract from data or use default value 0
  if (!is.null(tstart_var)) {
    tstart_var <- rlang::ensym(tstart_var)
    if (!rlang::as_name(tstart_var) %in% names(data)) {
      cli::cli_abort(
        "{.arg tstart_var} column '{rlang::as_name(tstart_var)}' not found in {.arg data}"
      )
    }
    tstart <- data[[rlang::as_name(tstart_var)]]
  } else {
    tstart <- 0
  }

  # Extract vectors from data
  id <- data[[rlang::as_name(id_var)]]
  time <- data[[rlang::as_name(time_var)]]
  cause <- data[[rlang::as_name(cause_var)]]

  # Validate that cause_var only contains values 0, 1, or 2
  cause_values <- unique(cause)
  invalid_values <- setdiff(cause_values, c(0, 1, 2))

  if (length(invalid_values) > 0) {
    cli::cli_abort(c(
      "{.arg cause_var} must only contain values 0, 1, or 2",
      "x" = "Found invalid values: {invalid_values}"
    ))
  }

  # Validate time and tstart value pairs
  if (any(time <= tstart, na.rm = TRUE)) {
    problematic_indices <- which(time <= tstart)
    sample_issues <- head(problematic_indices, 5)

    cli::cli_abort(c(
      "Found {length(problematic_indices)} case{?s} where event time is not greater than start time.",
      "i" = "First indices with issues: {sample_issues}",
      "i" = "Ensure all event times are strictly greater than start times."
    ))
  }

  # If no events of interest, return default value
  if (sum(cause == 1) == 0) {
    max_time <- max(time, na.rm = TRUE)
    cli::cli_warn(
      c(
        "{.arg cause_var} variable includes {.vals {unique(cause)}} only",
        "i" = "Setting sum of cumulative incidence to {.val {as.numeric(0)}} at maximum time point {.val {max_time}}"
      ),
      wrap = TRUE
    )

    return(
      tibble::tibble(
        time = max_time,
        SumCIs = 0
      )
    )
  }

  # Handle tstart as a vector if it's a single value
  if (length(tstart) == 1) {
    tstart <- rep(tstart, length(time))
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
    dplyr::arrange(id, time, dplyr::desc(cause1))

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
    dplyr::select(id, first) |>
    dplyr::rename(maxE = first) |>
    dplyr::ungroup()

  # Merge back to main data
  all_data <- dplyr::left_join(input_data, event_number, by = "id")

  # Create dataset for cumulative incidence calculations
  data_mcc <- NULL

  # Process first events
  data_temp <- all_data |>
    dplyr::filter(first == 1) |>
    dplyr::mutate(m_event = 1)
  data_mcc <- data_temp

  # Process subsequent events
  for (i in 2:max_events) {
    # For those with i or more records, take the ith record
    the_ith <- all_data |> dplyr::filter(first == i)

    # For those with <i records, take the last record
    # If the last record is an event 1, change it to 0
    the_last <- all_data |>
      dplyr::filter(maxE < i, first == maxE) |>
      dplyr::mutate(cause = ifelse(cause == 1, 0, cause))

    ci_data_ith <- dplyr::bind_rows(the_ith, the_last) |>
      dplyr::mutate(m_event = i)

    data_mcc <- dplyr::bind_rows(data_mcc, ci_data_ith)
  }

  # Calculate cumulative incidence for each recurrence level
  mcc_base <- NULL

  for (j in 1:max_events) {
    data_j <- data_mcc |> dplyr::filter(m_event == j)

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
          weights = weight.cens * weight.trunc
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

      # Calculate incremental changes and add recurrence indicator
      cm1 <- cm1 |>
        dplyr::mutate(
          Deta = c(cm[1], diff(cm)),
          cumI = j
        )

      mcc_base <- dplyr::bind_rows(mcc_base, cm1)
    }
  }

  # Remove duplicates and sort by event dates
  if (!is.null(mcc_base)) {
    nodup_mcc_base <- mcc_base |>
      dplyr::distinct(cm, cumI, .keep_all = TRUE)

    sort_mcc_base <- nodup_mcc_base |>
      dplyr::arrange(Time)

    # Calculate MCC by summing cumulative incidences
    mcc_values <- cumsum(sort_mcc_base$Deta)

    # Combine results
    combine_mcc <- sort_mcc_base |>
      dplyr::mutate(MCC = mcc_values)

    # Create final output: max MCC value at each time point
    mcc_final <- combine_mcc |>
      dplyr::group_by(Time) |>
      dplyr::summarize(SumCIs = max(MCC), .groups = "drop") |>
      dplyr::rename(time = Time)

    return(mcc_final)
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

    return(
      tibble::tibble(
        time = max_time,
        SumCIs = 0
      )
    )
  }
}
