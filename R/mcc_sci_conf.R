#' Calculate Mean Cumulative Count with bootstrap confidence intervals
#'
#' @param id Vector of participant IDs
#' @param time Vector of follow-up or event times
#' @param cause Vector of event indicators (1=event of interest, 2=competing risk, 0=censoring)
#' @param tstart Vector with start times of follow-up (default: 0, for no left truncation)
#' @param n_boot Number of bootstrap replications (default: 1000)
#' @param conf_level Confidence level (default: 0.95)
#' @param ci_type Type of confidence interval to calculate. Options include "perc" (percentile),
#'   "norm" (normal approximation), "basic" (basic bootstrap), "bca" (bias-corrected and accelerated),
#'   or "all" to compute all types (default: "perc")
#' @param parallel Whether to use parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing
#'
#' @return A list with two elements:
#'   \item{mcc_ci}{A tibble with columns for time, MCC, and confidence intervals}
#'   \item{boot_results}{The bootstrap results object}
#'
#' @export
mcc_sci_ci <- function(
  id,
  time,
  cause,
  tstart = 0,
  n_boot = 1000,
  conf_level = 0.95,
  ci_type = "perc",
  parallel = FALSE,
  n_cores = parallelly::availableCores(omit = 1)
) {
  # Input validation
  if (length(id) != length(time) || length(id) != length(cause)) {
    cli::cli_abort(
      "{.arg id}, {.arg time}, and {.arg cause} must have the same length"
    )
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

  # Validate CI type
  valid_ci_types <- c("norm", "basic", "perc", "bca", "all")
  if (!ci_type %in% valid_ci_types) {
    cli::cli_abort(c(
      "Invalid confidence interval type: {.val {ci_type}}",
      "i" = "Valid types are: {.val {valid_ci_types}}"
    ))
  }

  # First calculate MCC on the original data
  original_mcc <- mcc_sci(id = id, time = time, cause = cause, tstart = tstart)

  # Create input data frame
  data <- data.frame(
    id = id,
    time = time,
    cause = cause,
    tstart = if (length(tstart) == 1) rep(tstart, length(id)) else tstart
  )

  # Get unique IDs for sampling
  unique_ids <- unique(id)

  # Define bootstrap function
  boot_stat <- function(data, indices) {
    # Sample unique IDs with replacement
    sampled_ids <- unique_ids[indices]

    # Create a data frame with the mapping from original to new IDs
    id_mapping <- data.frame(
      original_id = sampled_ids,
      new_id = seq_along(sampled_ids)
    )

    # Create bootstrap dataset using dplyr
    boot_data <- data |>
      dplyr::filter(id %in% sampled_ids) |>
      dplyr::left_join(id_mapping, by = c("id" = "original_id")) |>
      dplyr::mutate(id = new_id) |>
      dplyr::select(-new_id)

    # Calculate MCC on bootstrap sample
    tryCatch(
      {
        boot_mcc <- mcc_sci(
          id = boot_data$id,
          time = boot_data$time,
          cause = boot_data$cause,
          tstart = boot_data$tstart
        )

        # Process results with dplyr
        result <- data.frame(time = original_mcc$time) |>
          dplyr::left_join(boot_mcc, by = "time") |>
          dplyr::arrange(time) |>
          dplyr::mutate(SumCIs = ifelse(is.na(SumCIs), 0, SumCIs)) |>
          dplyr::mutate(SumCIs = zoo::na.locf(SumCIs))

        return(result$SumCIs)
      },
      error = function(e) {
        cli::cli_alert_warning("Bootstrap error: {e$message}")
        return(rep(NA, nrow(original_mcc)))
      }
    )
  }

  # Setup for parallel processing
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      cli::cli_alert_warning(
        "Package 'parallel' is required for parallel processing."
      )
      cli::cli_alert_info("Switching to sequential mode.")
      parallel <- FALSE
    } else {
      cli::cli_alert_info("Running bootstrap with {n_cores} cores in parallel.")
      parallel_type <- if (.Platform$OS.type == "windows") "snow" else
        "multicore"

      # Make sure all required packages are loaded on each core
      if (parallel_type == "snow") {
        # For snow clusters, we need to export variables and ensure packages are loaded
        parallel_cluster <- parallel::makeCluster(n_cores)

        # Export required objects to the cluster
        parallel::clusterExport(
          parallel_cluster,
          c("original_mcc", "unique_ids", "mcc_sci"),
          envir = environment()
        )

        # Load required packages on each node
        parallel::clusterEvalQ(parallel_cluster, {
          library(data.table)
          library(dplyr)
          library(zoo)
        })

        # Make sure to stop the cluster when the function exits
        on.exit(parallel::stopCluster(parallel_cluster), add = TRUE)
      }
    }
  }

  # Create wrapper function for bootstrap with cli progress bar
  boot_with_cli_progress <- function(data, statistic, R, ...) {
    # Helper function to create a properly scoped environment for the cli progress bar
    run_with_progress <- function() {
      # Initialize progress bar
      cli::cli_alert_info("Starting bootstrap with {R} iterations...")

      # Start timer
      tictoc::tic("Bootstrap timer")

      # Create the progress bar with an ID
      pb_id <- cli::cli_progress_bar(
        format = "Bootstrapping {cli::pb_current}/{(cli::pb_total - 1)} iterations {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}",
        total = R + 1 # +1 to account for original sample
      )

      # Define a wrapper function that updates the progress bar
      stat_wrapper <- function(data, i) {
        res <- statistic(data, i)
        cli::cli_progress_update(id = pb_id)
        return(res)
      }

      # Call boot with the wrapper
      result <- boot::boot(data = data, statistic = stat_wrapper, R = R, ...)

      # Mark progress as complete
      cli::cli_progress_done(id = pb_id)
      bs_time <- tictoc::toc(quiet = TRUE)$callback_msg
      cli::cli_alert_info("{bs_time}")

      return(result)
    }

    # Run the function in a clean environment
    run_with_progress()
  }

  # Define bootstrap function that is completely self-contained
  boot_stat <- function(data, indices) {
    # Capture required variables from the parent environment
    if (!exists("original_mcc"))
      original_mcc <- get("original_mcc", envir = .GlobalEnv)
    if (!exists("unique_ids"))
      unique_ids <- get("unique_ids", envir = .GlobalEnv)

    # Load required packages if not already loaded
    if (!requireNamespace("data.table", quietly = TRUE)) {
      library(data.table)
    }
    if (!requireNamespace("zoo", quietly = TRUE)) {
      library(zoo)
    }

    # Sample unique IDs with replacement
    sampled_ids <- unique_ids[indices]

    # Create data.table from data frame if needed
    dt <- data.table::as.data.table(data)

    # Filter to rows with sampled IDs
    boot_dt <- dt[id %in% sampled_ids]

    # Create mapping of original IDs to new IDs
    id_map <- data.frame(
      original_id = sampled_ids,
      new_id = seq_along(sampled_ids)
    )

    # Replace IDs using a loop
    for (i in seq_along(unique(sampled_ids))) {
      original <- unique(sampled_ids)[i]
      new_id <- i
      boot_dt[id == original, id := new_id]
    }

    # Calculate MCC on bootstrap sample
    tryCatch(
      {
        boot_mcc <- mcc_sci(
          id = boot_dt$id,
          time = boot_dt$time,
          cause = boot_dt$cause,
          tstart = boot_dt$tstart
        )

        # Convert to data.table for merging
        result_dt <- data.table::data.table(time = original_mcc$time)
        boot_mcc_dt <- data.table::as.data.table(boot_mcc)

        # Use proper data.table merge syntax
        result <- merge(result_dt, boot_mcc_dt, by = "time", all.x = TRUE)
        result <- result[order(result$time), ]

        # Initialize SumCIs column with zeros for NA values at the beginning
        if ("SumCIs" %in% names(result)) {
          # Carry forward last observation
          result[, SumCIs := zoo::na.locf(SumCIs)]
        } else {
          # If no SumCIs column exists, return all zeros
          result[, SumCIs := 0]
        }

        return(result$SumCIs)
      },
      error = function(e) {
        # Return NAs if something went wrong
        return(rep(NA, nrow(original_mcc)))
      }
    )
  }

  # Run bootstrap
  if (parallel) {
    cli::cli_alert_info(
      "Starting {n_boot} bootstrap iterations in parallel mode..."
    )

    # Start timer for parallel bootstrap
    tictoc::tic("Parallel bootstrap timer")

    if (.Platform$OS.type == "windows") {
      # On Windows, use the snow cluster we prepared earlier
      boot_results <- boot::boot(
        data = data,
        statistic = boot_stat,
        R = n_boot,
        parallel = "snow",
        ncpus = n_cores,
        cl = parallel_cluster # Use the cluster we created
      )
    } else {
      # On Unix-like systems, use multicore
      boot_results <- boot::boot(
        data = data,
        statistic = boot_stat,
        R = n_boot,
        parallel = "multicore",
        ncpus = n_cores
      )
    }

    # Stop timer and report elapsed time
    bs_time <- tictoc::toc(quiet = TRUE)$callback_msg
    cli::cli_alert_info("{bs_time}")

    cli::cli_alert_success("Completed parallel bootstrap.")
  } else {
    # Use cli progress bar in sequential mode
    boot_results <- boot_with_cli_progress(
      data = data,
      statistic = boot_stat,
      R = n_boot
    )
  }

  # Process bootstrap results
  cli::cli_alert_info("Processing bootstrap results...")

  # Determine which CI types to compute
  ci_types_to_compute <- if (ci_type == "all") {
    c("norm", "basic", "perc", "bca")
  } else {
    ci_type
  }

  # Initialize results storage
  ci_results <- list()
  for (type in ci_types_to_compute) {
    ci_results[[type]] <- matrix(NA, nrow = nrow(original_mcc), ncol = 2)
  }

  # Calculate CIs for each time point
  warnings_encountered <- FALSE
  for (i in 1:nrow(original_mcc)) {
    # Skip time points where all bootstrap values are NA
    if (all(is.na(boot_results$t[, i]))) {
      warnings_encountered <- TRUE
      next
    }

    # For time points with some NAs but not all, fill with median
    if (any(is.na(boot_results$t[, i]))) {
      boot_vals <- boot_results$t[, i]
      med_val <- stats::median(boot_vals, na.rm = TRUE)
      boot_results$t[is.na(boot_results$t[, i]), i] <- med_val
    }

    # Check if we have sufficient variation for CIs
    if (length(unique(boot_results$t[, i])) <= 1) {
      # For constant values, just set CI to the constant value
      constant_value <- unique(boot_results$t[, i])[1]
      for (type in ci_types_to_compute) {
        ci_results[[type]][i, ] <- rep(constant_value, 2)
      }
      next
    }

    # Try to compute bootstrap CIs
    tryCatch(
      {
        # Process one CI type at a time
        for (type in ci_types_to_compute) {
          ci_result <- boot::boot.ci(
            boot_results,
            type = type, # Only one type at a time
            index = i,
            conf = conf_level
          )

          # Extract results based on CI type
          if (type == "norm") {
            if (!is.null(ci_result$normal)) {
              ci_results[[type]][i, ] <- ci_result$normal[2:3]
            }
          } else if (type == "basic") {
            if (!is.null(ci_result$basic)) {
              ci_results[[type]][i, ] <- ci_result$basic[4:5]
            }
          } else if (type == "perc") {
            if (!is.null(ci_result$percent)) {
              ci_results[[type]][i, ] <- ci_result$percent[4:5]
            }
          } else if (type == "bca") {
            if (!is.null(ci_result$bca)) {
              ci_results[[type]][i, ] <- ci_result$bca[4:5]
            }
          }
        }
      },
      error = function(e) {
        warnings_encountered <- TRUE
        cli::cli_alert_warning(
          "Error calculating confidence intervals for time point {i}: {e$message}"
        )
      }
    )
  }

  # Create result tibbles for each CI type
  result_tibbles <- list()

  # Default to percentile CI type if all failed or if "all" was requested
  default_ci_type <- "perc"
  if (ci_type == "all") ci_type <- default_ci_type

  # Build results for each CI type
  for (type in ci_types_to_compute) {
    result_tibbles[[type]] <- original_mcc |>
      dplyr::mutate(
        lcl = ci_results[[type]][, 1],
        ucl = ci_results[[type]][, 2],
        ci_type = type
      )
  }

  # Combine results if multiple CI types were requested
  if (length(ci_types_to_compute) > 1) {
    result_with_ci <- dplyr::bind_rows(result_tibbles)
  } else {
    result_with_ci <- result_tibbles[[ci_type]]
  }

  # Check for NaN warnings in confidence intervals
  if (
    warnings_encountered ||
      any(is.na(result_with_ci$lcl)) ||
      any(is.na(result_with_ci$ucl))
  ) {
    cli::cli_alert_warning(
      "Some confidence intervals could not be calculated due to insufficient variation in bootstrap samples."
    )
    cli::cli_alert_info(
      "Consider increasing the number of bootstrap iterations or using a different CI type."
    )
  }

  # Return results
  cli::cli_alert_success(
    "Bootstrap analysis complete with {n_boot} iterations."
  )
  return(list(
    "mcc_ci" = result_with_ci,
    "boot_results" = boot_results
  ))
}
