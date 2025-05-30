#' Check for and handle simultaneous events
#'
#' @param data_std Standardized data frame
#' @param adjust_times Whether to adjust times for simultaneous events
#' @param time_precision Precision for time adjustment
#'
#' @returns A list with adjusted data (if applicable) and a flag indicating if adjustments were made
#' @keywords internal
#' @noRd
handle_simultaneous_events <- function(data_std, adjust_times, time_precision) {
  # Convert to data.table for efficient operations
  dt <- data.table::as.data.table(data_std)
  data.table::setorder(dt, id, time)

  # Identify simultaneous events efficiently - check only once
  has_simultaneous_events <- dt[, any(.N > 1), by = .(id, time)][, any(V1)]

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
        cause == 1,
        1L,
        cause == 2,
        2L,
        cause == 0,
        3L,
        default = 4L # fallback for unexpected values
      )
    ]

    # Sort by id, time, then by cause priority
    data.table::setorder(dt, id, time, cause_priority)

    # Add row number within each (id, time) group to determine adjustment order
    dt[, row_within_time := seq_len(.N), by = .(id, time)]

    # Apply time adjustments only where needed (row_within_time > 1) - vectorized
    adjustment_needed <- dt$row_within_time > 1L
    if (any(adjustment_needed)) {
      dt[
        adjustment_needed,
        time := time + time_precision * (row_within_time - 1L)
      ]
    }

    # Clean up helper columns
    dt[, c("cause_priority", "row_within_time") := NULL]

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


#' Add group column to result tibbles
#'
#' @param result Result list from MCC calculation
#' @param by_name Name of the grouping variable
#' @param group_value Value of the group
#' @returns Modified result list with group column added
#' @keywords internal
#' @noRd
add_group_column_to_result <- function(result, by_name, group_value) {
  # Components that are tibbles and should get the group column
  tibble_components <- c(
    "mcc_final",
    "mcc_table",
    "sci_table",
    "mcc_base",
    "original_data",
    "adjusted_data"
  )

  for (component in tibble_components) {
    if (
      component %in%
        names(result) &&
        inherits(result[[component]], "data.frame")
    ) {
      # Add group column as the first column
      result[[component]] <- tibble::add_column(
        result[[component]],
        !!rlang::sym(by_name) := group_value,
        .before = 1
      )
    }
  }

  return(result)
}

#' Combine results from multiple groups
#'
#' @param group_results List of results from each group
#' @param by_name Name of the grouping variable
#' @param include_details Whether detailed results were requested
#' @returns Combined result list
#' @keywords internal
#' @noRd
combine_group_results <- function(group_results, by_name, include_details) {
  # Get the first non-null result to determine structure
  first_result <- group_results[[1]]

  # Initialize combined result
  combined_result <- list()

  # Components to combine (always present)
  always_combine <- c("mcc_final")

  # Components to combine if include_details = TRUE
  detail_components <- c(
    "mcc_table",
    "sci_table",
    "mcc_base",
    "original_data",
    "adjusted_data"
  )

  # Determine which components to combine
  if (include_details) {
    components_to_combine <- c(always_combine, detail_components)
  } else {
    components_to_combine <- always_combine
  }

  # Combine tibble components
  for (component in components_to_combine) {
    if (component %in% names(first_result)) {
      # Extract this component from all groups that have it
      component_list <- lapply(group_results, function(x) {
        if (component %in% names(x)) x[[component]] else NULL
      })

      # Remove NULL entries
      component_list <- component_list[!sapply(component_list, is.null)]

      if (length(component_list) > 0) {
        # For sci_table specifically, we need to handle different CI columns
        if (component == "sci_table") {
          # Get all unique column names across all sci_tables
          all_cols <- unique(unlist(lapply(component_list, names)))

          # Ensure all tibbles have the same columns by adding missing ones with NA
          component_list <- lapply(component_list, function(tbl) {
            missing_cols <- setdiff(all_cols, names(tbl))
            if (length(missing_cols) > 0) {
              for (col in missing_cols) {
                tbl[[col]] <- NA_real_ # Use appropriate NA type
              }
            }
            # Reorder columns to match
            tbl[, all_cols]
          })
        }

        # Combine all tibbles using rbind
        combined_result[[component]] <- do.call(rbind, component_list)
      }
    }
  }

  # Handle all_cis specially (it's a list of lists)
  if (include_details && "all_cis" %in% names(first_result)) {
    # Create a nested list structure: group -> event_number -> tibble
    combined_all_cis <- list()

    for (group_name in names(group_results)) {
      if ("all_cis" %in% names(group_results[[group_name]])) {
        group_all_cis <- group_results[[group_name]]$all_cis

        # Add group column to each tibble in all_cis
        for (i in seq_along(group_all_cis)) {
          if (nrow(group_all_cis[[i]]) > 0) {
            group_all_cis[[i]] <- tibble::add_column(
              group_all_cis[[i]],
              !!rlang::sym(by_name) := group_name,
              .before = 1
            )
          }
        }

        combined_all_cis[[group_name]] <- group_all_cis
      }
    }

    combined_result$all_cis <- combined_all_cis
  }

  # Handle se_info specially - it should be the same across all groups
  if ("se_info" %in% names(first_result)) {
    combined_result$se_info <- first_result$se_info
  }

  return(combined_result)
}

#' Calculate analytical standard errors for MCC using Ghosh-Lin method
#'
#' @param mcc_table The detailed MCC calculation table
#' @param original_data The original standardized data
#' @param n_total Total number of participants
#' @param method Method for confidence intervals ("normal", "log", "logit")
#' @param conf_level Confidence level (default 0.95)
#'
#' @returns A tibble with MCC estimates, standard errors, and confidence intervals
#' @keywords internal
#' @noRd
calculate_analytical_se <- function(
  mcc_table,
  original_data,
  n_total,
  method = "log",
  conf_level = 0.95
) {
  # Calculate individual contributions G_i(s,t) for each subject
  # This implements equation (2.2) from Ghosh & Lin (2000)

  # Get unique IDs and time points
  unique_ids <- unique(original_data$id)
  times <- mcc_table$time

  # Pre-allocate matrix for G_i contributions
  g_matrix <- matrix(0, nrow = length(unique_ids), ncol = length(times))

  # Calculate G_i for each individual at each time point
  for (i in seq_along(unique_ids)) {
    id_data <- original_data[original_data$id == unique_ids[i], ]

    for (j in seq_along(times)) {
      t <- times[j]
      g_matrix[i, j] <- calculate_g_i(id_data, t, mcc_table)
    }
  }

  # Calculate variance-covariance matrix
  variance_matrix <- calculate_variance_matrix(g_matrix, n_total)

  # Extract standard errors (diagonal elements)
  standard_errors <- sqrt(diag(variance_matrix))

  # Calculate confidence intervals
  ci_results <- calculate_confidence_intervals(
    mcc_table$mcc,
    standard_errors,
    method = method,
    conf_level = conf_level
  )

  # Combine results
  result <- mcc_table |>
    dplyr::mutate(
      se = standard_errors,
      lower_ci = ci_results$lower,
      upper_ci = ci_results$upper,
      ci_method = method,
      conf_level = conf_level
    )

  return(result)
}

#' Calculate G_i contribution for individual i at time t
#'
#' This implements the G_i(s,t) function from equation (2.2) in Ghosh & Lin (2000)
#'
#' @param id_data Data for a single individual
#' @param t Time point of interest
#' @param mcc_table MCC calculation table
#'
#' @returns Numeric value of G_i contribution
#' @keywords internal
#' @noRd
calculate_g_i <- function(id_data, t, mcc_table) {
  # Handle edge cases
  if (nrow(id_data) == 0 || nrow(mcc_table) == 0) {
    return(0)
  }

  # Get the MCC value at time t
  mcc_at_t_indices <- which(mcc_table$time <= t)
  if (length(mcc_at_t_indices) == 0) {
    return(0)
  }

  mcc_t <- mcc_table$mcc[max(mcc_at_t_indices)]
  if (is.na(mcc_t) || !is.finite(mcc_t)) {
    mcc_t <- 0
  }

  # Calculate components based on Ghosh-Lin formula
  # Component 1: Events experienced by individual i up to time t
  events_at_or_before_t <- id_data$time <= t &
    id_data$cause == 1 &
    !is.na(id_data$time) &
    !is.na(id_data$cause)
  events_component <- sum(events_at_or_before_t, na.rm = TRUE)

  # Component 2: At-risk indicator and hazard contributions
  # This requires integration over the at-risk period
  at_risk_component <- 0

  # Check if individual has any competing risk events before time t
  competing_before_t <- any(
    id_data$time <= t & id_data$cause == 2,
    na.rm = TRUE
  )

  for (k in seq_len(nrow(mcc_table))) {
    time_k <- mcc_table$time[k]
    if (is.na(time_k) || time_k > t) break

    # Check if individual i is at risk at time_k
    # Individual is at risk if:
    # 1. They have observations at or after time_k
    # 2. They haven't had a competing risk before time_k
    has_obs_at_or_after <- any(id_data$time >= time_k, na.rm = TRUE)
    has_competing_before <- any(
      id_data$time < time_k & id_data$cause == 2,
      na.rm = TRUE
    )

    is_at_risk <- has_obs_at_or_after && !has_competing_before

    if (is_at_risk && !is.na(mcc_table$nrisk[k]) && mcc_table$nrisk[k] > 0) {
      # Add contribution based on hazard and survival probability
      hazard_contrib <- 0
      if (!is.na(mcc_table$event[k]) && is.finite(mcc_table$event[k])) {
        hazard_contrib <- mcc_table$event[k] / mcc_table$nrisk[k]
      }

      surv_prob <- mcc_table$overall_surv_previous[k]
      if (is.na(surv_prob) || !is.finite(surv_prob)) {
        surv_prob <- 1
      }

      contribution <- surv_prob * hazard_contrib
      if (is.finite(contribution)) {
        at_risk_component <- at_risk_component + contribution
      }
    }
  }

  # Component 3: Competing risk adjustments
  competing_component <- 0
  competing_events <- id_data[
    id_data$cause == 2 &
      id_data$time <= t &
      !is.na(id_data$cause) &
      !is.na(id_data$time),
  ]

  if (nrow(competing_events) > 0) {
    for (ce_idx in seq_len(nrow(competing_events))) {
      ce_time <- competing_events$time[ce_idx]
      if (is.na(ce_time)) next

      # Find corresponding MCC table entry
      mcc_idx <- which(abs(mcc_table$time - ce_time) < 1e-10) # Handle floating point comparison
      if (length(mcc_idx) > 0) {
        mcc_val <- mcc_table$mcc[mcc_idx[1]]
        nrisk_val <- mcc_table$nrisk[mcc_idx[1]]
        cmprk_val <- mcc_table$cmprk[mcc_idx[1]]

        if (
          !is.na(mcc_val) &&
            !is.na(nrisk_val) &&
            !is.na(cmprk_val) &&
            nrisk_val > 0 &&
            is.finite(mcc_val) &&
            is.finite(cmprk_val)
        ) {
          contribution <- mcc_val * (cmprk_val / nrisk_val)
          if (is.finite(contribution)) {
            competing_component <- competing_component + contribution
          }
        }
      }
    }
  }

  # Combine components (this is a simplified version - full implementation
  # would need more careful handling of the integral terms)
  g_i <- events_component - at_risk_component - competing_component - mcc_t

  # Ensure finite result
  if (!is.finite(g_i)) {
    g_i <- 0
  }

  return(g_i)
}

#' Calculate variance-covariance matrix
#'
#' @param g_matrix Matrix of G_i contributions (n x p)
#' @param n_total Total sample size
#'
#' @returns Variance-covariance matrix
#' @keywords internal
#' @noRd
calculate_variance_matrix <- function(g_matrix, n_total) {
  # Calculate sample covariance matrix and scale by n
  # This implements the variance estimator from Ghosh & Lin (2000)

  # Center the G_i values (should have mean 0 asymptotically)
  g_centered <- sweep(g_matrix, 2, colMeans(g_matrix), "-")

  # Calculate variance-covariance matrix
  # Var(MCC(t)) = (1/n) * sum(G_i(t) * G_i(t)')
  variance_matrix <- (1 / n_total) * t(g_centered) %*% g_centered

  return(variance_matrix)
}

#' Calculate confidence intervals for MCC estimates
#'
#' @param mcc_estimates Vector of MCC estimates
#' @param standard_errors Vector of standard errors
#' @param method Method for CI calculation ("normal", "log", "logit")
#' @param conf_level Confidence level
#'
#' @returns List with lower and upper confidence bounds
#' @keywords internal
#' @noRd
calculate_confidence_intervals <- function(
  mcc_estimates,
  standard_errors,
  method = "log",
  conf_level = 0.95
) {
  alpha <- 1 - conf_level
  z_alpha <- qnorm(1 - alpha / 2)

  # Initialize with NA values
  lower <- rep(NA_real_, length(mcc_estimates))
  upper <- rep(NA_real_, length(mcc_estimates))

  # Only calculate CIs for finite, non-NA values
  valid_indices <- !is.na(mcc_estimates) &
    !is.na(standard_errors) &
    is.finite(mcc_estimates) &
    is.finite(standard_errors)

  if (!any(valid_indices)) {
    return(list(lower = lower, upper = upper))
  }

  if (method == "normal") {
    # Normal approximation
    lower[valid_indices] <- mcc_estimates[valid_indices] -
      z_alpha * standard_errors[valid_indices]
    upper[valid_indices] <- mcc_estimates[valid_indices] +
      z_alpha * standard_errors[valid_indices]

    # Ensure non-negative bounds
    lower[valid_indices] <- pmax(lower[valid_indices], 0)
  } else if (method == "log") {
    # Log transformation (recommended in Ghosh & Lin for better properties)
    # Only apply where MCC > 0 and values are valid
    log_applicable <- valid_indices &
      mcc_estimates > 0 &
      !is.na(mcc_estimates > 0)

    # Set default values for non-applicable cases
    lower[valid_indices] <- 0
    upper[valid_indices] <- Inf

    # Handle zero or negative MCC values
    zero_mcc <- valid_indices & mcc_estimates <= 0 & !is.na(mcc_estimates <= 0)
    if (any(zero_mcc, na.rm = TRUE)) {
      lower[zero_mcc] <- 0
      upper[zero_mcc] <- 2 * z_alpha * standard_errors[zero_mcc] # Approximate upper bound
    }

    # Apply log transformation where applicable
    if (any(log_applicable, na.rm = TRUE)) {
      log_mcc <- log(mcc_estimates[log_applicable])
      log_se <- standard_errors[log_applicable] / mcc_estimates[log_applicable]

      # Check for finite log standard errors
      finite_log_se <- is.finite(log_se) & !is.na(log_se)

      if (any(finite_log_se)) {
        valid_log_indices <- which(log_applicable)[finite_log_se]

        log_lower <- log_mcc[finite_log_se] - z_alpha * log_se[finite_log_se]
        log_upper <- log_mcc[finite_log_se] + z_alpha * log_se[finite_log_se]

        lower[valid_log_indices] <- exp(log_lower)
        upper[valid_log_indices] <- exp(log_upper)
      }
    }
  } else if (method == "logit") {
    # Logit transformation (for bounded outcomes)
    # This would need maximum possible MCC value - not typically used for MCC
    stop("Logit transformation not implemented for MCC - use 'normal' or 'log'")
  } else {
    stop("Method must be one of: 'normal', 'log', 'logit'")
  }

  return(list(lower = lower, upper = upper))
}

#' Enhanced MCC equation method with analytical standard errors
#'
#' @inheritParams mcc_equation
#' @param calculate_se Whether to calculate analytical standard errors
#' @param se_method Method for confidence intervals ("normal", "log")
#' @param conf_level Confidence level for confidence intervals
#'
#' @returns Enhanced MCC results with standard errors and confidence intervals
#' @keywords internal
#' @noRd
mcc_equation_with_se <- function(
  data,
  id_var,
  time_var,
  cause_var,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE,
  calculate_se = FALSE,
  se_method = "log",
  conf_level = 0.95
) {
  # First calculate standard MCC
  mcc_result <- mcc_equation(
    data = data,
    id_var = {{ id_var }},
    time_var = {{ time_var }},
    cause_var = {{ cause_var }},
    adjust_times = adjust_times,
    time_precision = time_precision,
    include_details = TRUE # Always need details for SE calculation
  )

  # Calculate analytical standard errors if requested
  if (calculate_se) {
    # Validate inputs
    if (!se_method %in% c("normal", "log")) {
      cli::cli_abort(c(
        "{.arg se_method} must be one of: {.val normal} or {.val log}",
        "x" = "Received: {.val {se_method}}"
      ))
    }

    if (
      !is.numeric(conf_level) ||
        length(conf_level) != 1 ||
        conf_level <= 0 ||
        conf_level >= 1
    ) {
      cli::cli_abort(c(
        "{.arg conf_level} must be a single number between 0 and 1",
        "x" = "Received: {.val {conf_level}}"
      ))
    }

    # Calculate total number of unique participants
    n_total <- length(unique(mcc_result$original_data$id))

    # Calculate analytical standard errors
    mcc_with_se <- calculate_analytical_se(
      mcc_table = mcc_result$mcc_table,
      original_data = mcc_result$original_data,
      n_total = n_total,
      method = se_method,
      conf_level = conf_level
    )

    # Update mcc_table with SE information
    mcc_result$mcc_table <- mcc_with_se

    # Update mcc_final with SE information for unique MCC values
    mcc_final_with_se <- mcc_with_se |>
      dplyr::group_by(mcc) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(time, mcc, se, lower_ci, upper_ci, ci_method, conf_level)

    mcc_result$mcc_final <- mcc_final_with_se

    # Add SE metadata
    mcc_result$se_info <- list(
      method = se_method,
      conf_level = conf_level,
      calculated = TRUE
    )

    cli::cli_alert_success(
      "Analytical standard errors calculated using Ghosh-Lin method"
    )
  }

  # Handle include_details parameter
  if (!include_details && !calculate_se) {
    # Simplified output without SE
    mcc_result <- list(mcc_final = mcc_result$mcc_final)
  } else if (!include_details && calculate_se) {
    # Simplified output with SE
    mcc_result <- list(
      mcc_final = mcc_result$mcc_final,
      se_info = mcc_result$se_info
    )
  }

  return(mcc_result)
}
