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
#'
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
                # Use 0 for CI columns and SumCIs, NA for other columns
                if (grepl("^CI\\d+$", col)) {
                  tbl[[col]] <- 0 # Cumulative incidence of 0 for non-occurring events
                } else {
                  tbl[[col]] <- NA_real_ # NA for other numeric columns
                }
              }
            }
            # Reorder columns to match
            tbl[, all_cols]
          })
        }

        # Combine all tibbles using rbindlist
        combined_result[[component]] <- data.table::rbindlist(component_list)
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

  return(combined_result)
}

#' Clean up tiny adjusted time values for user-facing output
#'
#' Converts times that are very close to time_precision back to 0.
#'    This handles the case where 0 was adjusted to time_precision for
#'    computation to prevent the first row of `time` column from being equal
#'    to `time_precision` instead of being equal to zero.
#'
#' @param data_table data_table of results from `method = "sci"`
#' @param time_precision time_precision used when calculating MCC to handle
#'     when `time == tstart_var== 0`
#'
#' @returns Updated table where time 0 is converted back to value of 0 if it
#'     was adjusted for computation reasons
#'
#' @keywords internal
#' @noRd
cleanup_output_times <- function(data_table, time_precision = 1e-6) {
  if ("time" %in% names(data_table)) {
    tiny_times <- abs(data_table$time - time_precision) <
      (.Machine$double.eps * 100)
    data_table[tiny_times, time := 0]
  }
  if ("Time" %in% names(data_table)) {
    tiny_times <- abs(data_table$Time - time_precision) <
      (.Machine$double.eps * 100)
    data_table[tiny_times, Time := 0]
  }

  return(data_table)
}
