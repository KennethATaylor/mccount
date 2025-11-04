#' Create an `mcc` S3 object
#'
#' @param result List containing MCC calculation results
#' @param method Method used for calculation ("equation" or "sci")
#' @param weighted Logical indicating if weighted estimation was used
#' @param by_group Optional name of grouping variable for grouped analyses
#' @param call The original function call
#'
#' @returns An S3 object of class "mcc" with appropriate subclasses
#' @keywords internal
#' @noRd
new_mcc <- function(
  result,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL
) {
  # Minimal validation for performance
  stopifnot(is.list(result))
  stopifnot(method %in% c("equation", "sci"))
  stopifnot(is.logical(weighted))

  # Add metadata to the result
  result$method <- method
  result$weighted <- weighted
  result$by_group <- by_group
  result$call <- call

  # Determine class hierarchy
  classes <- "mcc"

  # Add method-specific subclass
  if (method == "equation") {
    classes <- c("mcc_equation", classes)
  } else if (method == "sci") {
    classes <- c("mcc_sci", classes)
  }

  # Add weighted subclass if applicable
  if (weighted) {
    classes <- c("mcc_weighted", classes)
  }

  # Add grouped subclass if applicable
  if (!is.null(by_group)) {
    classes <- c("mcc_grouped", classes)
  }

  # Set the class
  class(result) <- classes

  return(result)
}

#' Validate `mcc` S3 objects
#'
#' @param x An object to validate as an MCC result
#'
#' @returns TRUE if valid, otherwise throws an error
#' @keywords internal
#' @noRd
validate_mcc <- function(x) {
  # Check if it's a list
  if (!is.list(x)) {
    cli::cli_abort("{.cls mcc} object must be a {.cls list}")
  }

  # Required components for all MCC objects
  required_components <- c("mcc_final", "method", "weighted")
  missing_components <- setdiff(required_components, names(x))

  if (length(missing_components) > 0) {
    cli::cli_abort(c(
      "{.cls mcc} object missing required components:",
      "x" = "Missing: {missing_components}"
    ))
  }

  # Validate mcc_final structure
  if (!is.data.frame(x$mcc_final)) {
    cli::cli_abort("{.field mcc_final} must be a {.cls data.frame} or tibble")
  }

  # Check for required columns in mcc_final
  mcc_final_cols <- names(x$mcc_final)
  if (!"time" %in% mcc_final_cols) {
    cli::cli_abort("{.field mcc_final} must contain a {.val time} column")
  }

  # Method-specific validation
  if (x$method == "equation") {
    if (!"mcc" %in% mcc_final_cols) {
      cli::cli_abort(
        "For equation method, {.field mcc_final} must contain an {.val mcc} column"
      )
    }
  } else if (x$method == "sci") {
    if (!"SumCIs" %in% mcc_final_cols) {
      cli::cli_abort(
        "For sci method, {.field mcc_final} must contain a {.val SumCIs} column"
      )
    }
  }

  # Validate method value
  if (!x$method %in% c("equation", "sci")) {
    cli::cli_abort(
      "{.field method} must be either {.val equation} or {.val sci}"
    )
  }

  # Validate weighted value
  if (!is.logical(x$weighted) || length(x$weighted) != 1) {
    cli::cli_abort("{.field weighted} must be a {.cls logical} value")
  }

  # Grouped-specific validation
  if (is_grouped(x)) {
    if (is.null(x$by_group)) {
      cli::cli_abort(
        "Grouped {.cls mcc} objects must have a {.field by_group} component"
      )
    }

    # Check that grouping column exists in mcc_final
    if (!x$by_group %in% names(x$mcc_final)) {
      cli::cli_abort(
        "Grouping variable '{x$by_group}' not found in {.field mcc_final}"
      )
    }
  }

  return(TRUE)
}

#' Construct an `mcc` S3 object (high-level constructor with validation)
#'
#' @param result List containing MCC calculation results
#' @param method Method used for calculation
#' @param weighted Logical indicating if weighted estimation was used
#' @param by_group Optional name of grouping variable
#' @param call The original function call
#'
#' @returns A validated `mcc` S3 object
#' @keywords internal
#' @noRd
mcc_object <- function(
  result,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL
) {
  # Create the object
  obj <- new_mcc(result, method, weighted, by_group, call)

  # Validate it thoroughly
  validate_mcc(obj)

  return(obj)
}

#' Convert objects to `mcc` class
#'
#' @description
#' Converts objects to MCC class. This is useful when you have calculation
#' results from other sources that you want to treat as MCC objects.
#'
#' @param x Object to convert to `mcc`
#' @param method Method used for calculation ("equation" or "sci")
#' @param weighted Logical indicating if weighted estimation was used
#' @param by_group Optional name of grouping variable
#' @param call Optional function call to store
#' @param ... Additional arguments (currently unused)
#'
#' @returns An `mcc` S3 object
#' @export
#'
#' @examples
#' # Convert a data.frame to MCC object
#' library(dplyr)
#'
#' # Create a simple data.frame with MCC results
#' mcc_data <- data.frame(
#'   time = c(1, 2, 3, 4, 5),
#'   mcc = c(0.1, 0.3, 0.5, 0.7, 1.0)
#' )
#'
#' # Convert to MCC object (equation method)
#' mcc_obj <- as_mcc(mcc_data, method = "equation")
#' print(mcc_obj)
#' is_mcc(mcc_obj)  # TRUE
#'
#' # Convert for SCI method (requires SumCIs column)
#' sci_data <- data.frame(
#'   time = c(1, 2, 3, 4, 5),
#'   SumCIs = c(0.1, 0.3, 0.5, 0.7, 1.0)
#' )
#'
#' mcc_sci_obj <- as_mcc(sci_data, method = "sci")
#' print(mcc_sci_obj)
#'
#' # Convert a list to MCC object
#' mcc_list <- list(
#'   mcc_final = data.frame(
#'     time = c(1, 2, 3),
#'     mcc = c(0.2, 0.5, 0.8)
#'   )
#' )
#'
#' mcc_from_list <- as_mcc(mcc_list, method = "equation")
#' print(mcc_from_list)
#'
#' # Clean up
#' rm(mcc_data, sci_data, mcc_list, mcc_obj, mcc_sci_obj, mcc_from_list)
#'
as_mcc <- function(
  x,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL,
  ...
) {
  UseMethod("as_mcc")
}

#' @export
as_mcc.default <- function(
  x,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL,
  ...
) {
  cli::cli_abort(c(
    "Don't know how to convert object of class {.cls {class(x)}} to {.cls mcc}",
    "i" = "Supported classes: {.cls list}, {.cls data.frame}"
  ))
}

#' @export
as_mcc.list <- function(
  x,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL,
  ...
) {
  # Validate that the list has the required structure for MCC
  if (!"mcc_final" %in% names(x)) {
    cli::cli_abort(c(
      "List must contain {.field mcc_final} component",
      "i" = "This should be a {.cls data.frame} with {.val time} and MCC estimate columns"
    ))
  }

  if (!is.data.frame(x$mcc_final)) {
    cli::cli_abort("{.field mcc_final} must be a {.cls data.frame} or tibble")
  }

  # Create and validate the MCC object
  mcc_obj <- mcc_object(x, method, weighted, by_group, call)

  return(mcc_obj)
}

#' @export
as_mcc.data.frame <- function(
  x,
  method,
  weighted = FALSE,
  by_group = NULL,
  call = NULL,
  ...
) {
  # Convert data.frame to the expected list structure

  # Validate required columns
  required_cols <- "time"
  mcc_col <- if (method == "equation") "mcc" else "SumCIs"
  required_cols <- c(required_cols, mcc_col)

  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Data.frame missing required columns:",
      "x" = "Missing: {missing_cols}",
      "i" = "For method '{method}', need: {required_cols}"
    ))
  }

  # Create the list structure
  result_list <- list(
    mcc_final = tibble::as_tibble(x)
  )

  # Create and validate the MCC object
  mcc_obj <- mcc_object(result_list, method, weighted, by_group, call)

  return(mcc_obj)
}

#' Check if object is an `mcc` result
#'
#' @param x An object to test
#'
#' @returns TRUE if x is an `mcc` object, FALSE otherwise
#' @export
#'
#' @examples
#' # Create sample data
#' library(dplyr)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
#'  ) |>
#'   arrange(id, time)
#'
#' # Calculate MCC
#' mcc_result <- mcc(df, "id", "time", "cause")
#'
#' # Test if it's an MCC object
#' is_mcc(mcc_result)  # TRUE
#'
#' # Clean up
#' rm(df, mcc_result)
#'
is_mcc <- function(x) {
  inherits(x, "mcc")
}

#' Print method for `mcc` objects
#'
#' @param x An `mcc` object
#' @param ... Additional arguments (currently unused)
#'
#' @returns x invisibly
#' @export
#'
#' @examples
#' # Attach dplyr
#' library(dplyr)
#' # Create sample data with recurrent events
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
#'  ) |>
#'   arrange(id, time)  # Sort the data by id and time
#'
#' # Calculate MCC using the equation method (default)
#' mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#'
#' # Print the S3 object (uses print.mcc method)
#' mcc_eq
#'
#' # Calculate MCC using the sum of cumulative incidence method
#' mcc_sci <- mcc(
#'   df,
#'   id_var = "id",
#'   time_var = "time",
#'   cause_var = "cause",
#'   method = "sci"
#' )
#'
#' # Print the S3 object
#' mcc_sci
#'
#' # Clean up
#' rm(df, mcc_eq, mcc_sci)
#'
print.mcc <- function(x, ...) {
  cli::cli_h1("Mean Cumulative Count Results")

  # Print method information
  method_label <- switch(
    x$method,
    "equation" = "Dong-Yasui Equation Method",
    "sci" = "Sum of Cumulative Incidence Method"
  )
  cli::cli_alert_info("Method: {.val {noquote(method_label)}}")

  if (x$weighted) {
    cli::cli_alert_info("Weighted estimation: {.val {noquote('Yes')}}")
  }

  # Print grouping information if applicable
  if (is_grouped(x)) {
    n_groups <- length(unique(x$mcc_final[[x$by_group]]))
    cli::cli_alert_info(
      "Grouped by: {.val {noquote(x$by_group)}} ({.val {as.numeric(n_groups)}} groups)"
    )
  }

  # Print sample of results
  cli::cli_h2("MCC Estimates")

  if (is_grouped(x)) {
    # For grouped results, show a sample from each group
    groups <- unique(x$mcc_final[[x$by_group]])
    n_show <- min(3, length(groups))

    for (i in seq_len(n_show)) {
      group_val <- groups[i]
      group_data <- x$mcc_final[x$mcc_final[[x$by_group]] == group_val, ]

      cli::cli_h3("Group: {group_val}")
      if (nrow(group_data) <= 6) {
        print(group_data)
      } else {
        print(utils::head(group_data, 3))
        cli::cli_text(
          "# ... with {.val {as.numeric(nrow(group_data) - 3)}} more rows"
        )
      }
    }

    if (length(groups) > n_show) {
      cli::cli_text(
        "# ... with {.val {as.numeric(length(groups) - n_show)}} more groups"
      )
    }
  } else {
    # For ungrouped results
    if (nrow(x$mcc_final) <= 10) {
      print(x$mcc_final)
    } else {
      print(utils::head(x$mcc_final, 6))
      cli::cli_text(
        "# ... with {.val {as.numeric(nrow(x$mcc_final) - 6)}} more rows"
      )
    }
  }

  # Print call if available
  if (!is.null(x$call)) {
    cli::cli_h2("Call")
    cli::cli_code(deparse(x$call))
  }

  invisible(x)
}

#' Get Time When MCC Reaches a Specific Threshold
#'
#' @description
#' Helper function that identifies the first time point when the Mean Cumulative
#' Count (MCC) reaches or exceeds the threshold. An MCC value of the threshold
#' represents the time when the population experiences an average of
#' `<threshold>` event(s).
#'
#' @param mcc_data A data frame containing MCC estimates over time. This is
#'   typically the `mcc_final` component from an `mcc` object.
#' @param mcc_column A string specifying the name of the column containing
#'   MCC values. For `method = "equation"`, this is typically `"mcc"`. For
#'   `method = "sci"`, this is typically `"SumCIs"`.
#' @param threshold numeric;determines MCC value threshold to use (default =
#'      `1.0`)
#'
#' @returns A numeric value representing the time when MCC first reaches or
#'   exceeds the `threshold`, or `NA_real_` if MCC never reaches `threshold`
#'   during the observed follow-up period.
#'
#' @details
#' The MCC represents the expected cumulative number of events per person
#' in the population initially at risk. When MCC = `threshold`, this indicates
#' that the population has experienced an average of 1 event per person. This
#' milestone can be useful for:
#'
#' - Identifying when the event burden reaches clinical or epidemiological
#'   significance
#' - Comparing event timing across different exposure groups or populations
#' - Setting thresholds for intervention planning
#'
#' Note that MCC values can exceed `threshold`, indicating more than `threshold`
#' number of events per person on average, which distinguishes it from
#' probability-based measures like cumulative incidence that are bounded
#' between 0 and 1.
#'
#' @keywords internal
#'
get_time_to_mcc <- function(mcc_data, mcc_column, threshold = 1.0) {
  # Find first time when MCC >= threshold
  mcc_one_rows <- which(mcc_data[[mcc_column]] >= threshold)
  if (length(mcc_one_rows) > 0) {
    return(mcc_data$time[min(mcc_one_rows)])
  } else {
    return(NA_real_)
  }
}

#' Summary method for `mcc` objects
#'
#' @param object An `mcc` object
#' @param ... Additional arguments (currently unused)
#'
#' @returns A summary object with class `summary.mcc`
#' @export
#'
#' @examples
#' # Attach dplyr
#' library(dplyr)
#' # Create sample data with recurrent events
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
#'  ) |>
#'   arrange(id, time)  # Sort the data by id and time
#'
#' # Calculate MCC using the equation method (default)
#' mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#'
#' summary(mcc_eq)
#'
#' # Calculate MCC using the sum of cumulative incidence method
#' mcc_sci <- mcc(
#'   df,
#'   id_var = "id",
#'   time_var = "time",
#'   cause_var = "cause",
#'   method = "sci"
#' )
#'
#' summary(mcc_sci)
#'
#' # Clean up
#' rm(df, mcc_eq, mcc_sci)
#'
summary.mcc <- function(object, ...) {
  # Extract key information
  mcc_col <- if (object$method == "equation") "mcc" else "SumCIs"

  # Helper function to get max follow-up time from original data
  get_max_followup_time <- function(obj) {
    if ("original_data" %in% names(obj) && !is.null(obj$original_data)) {
      # Get maximum time from original data (includes all censoring/competing risk times)
      return(max(obj$original_data$time, na.rm = TRUE))
    } else {
      # Fallback to max time in mcc_table if original_data not available
      return(max(obj$mcc_table$time, na.rm = TRUE))
    }
  }

  # Helper function to get event counts from original data
  get_event_counts <- function(obj) {
    if ("original_data" %in% names(obj) && !is.null(obj$original_data)) {
      data_to_use <- obj$original_data
    } else {
      # Fallback: try to get from other available data sources
      if ("mcc_table" %in% names(obj) && !is.null(obj$mcc_table)) {
        # For equation method, we don't have individual-level data in mcc_table
        # but we can try to reconstruct from the aggregated data
        return(list(
          n_participants = NA_integer_,
          n_events = NA_integer_,
          n_competing = NA_integer_,
          n_censoring = NA_integer_
        ))
      } else {
        return(list(
          n_participants = NA_integer_,
          n_events = NA_integer_,
          n_competing = NA_integer_,
          n_censoring = NA_integer_
        ))
      }
    }

    # Count unique participants
    n_participants <- length(unique(data_to_use$id))

    # Count events by cause type
    event_counts <- data_to_use |>
      dplyr::count(.data$cause, name = "n") |>
      dplyr::mutate(
        cause = factor(
          .data$cause,
          levels = c(0, 1, 2),
          labels = c("censoring", "events", "competing")
        )
      )

    # Create a complete summary with all event types (even if count is 0)
    complete_counts <- data.frame(
      cause = factor(
        c("censoring", "events", "competing"),
        levels = c("censoring", "events", "competing")
      ),
      n = 0L
    ) |>
      dplyr::left_join(event_counts, by = "cause") |>
      dplyr::mutate(n = dplyr::coalesce(.data$n.y, .data$n.x)) |>
      dplyr::select("cause", "n")

    return(list(
      n_participants = n_participants,
      n_events = complete_counts$n[complete_counts$cause == "events"],
      n_competing = complete_counts$n[complete_counts$cause == "competing"],
      n_censoring = complete_counts$n[complete_counts$cause == "censoring"]
    ))
  }

  # Helper function to get MCC at end of follow-up
  get_mcc_at_end_followup <- function(mcc_data, mcc_column, max_followup) {
    # Find the MCC value at the time closest to (but not exceeding) max follow-up
    valid_times <- mcc_data$time[mcc_data$time <= max_followup]
    if (length(valid_times) > 0) {
      max_valid_time <- max(valid_times)
      return(mcc_data[[mcc_column]][mcc_data$time == max_valid_time])
    } else {
      return(0) # If no events by end of follow-up
    }
  }

  if (is_grouped(object)) {
    # Grouped summary - calculate counts per group
    overall_max_followup_time <- get_max_followup_time(object)

    # Get group-specific event counts AND group-specific max follow-up times
    if ("original_data" %in% names(object) && !is.null(object$original_data)) {
      group_event_counts <- object$original_data |>
        dplyr::group_by(!!rlang::sym(object$by_group)) |>
        dplyr::summarise(
          n_participants = dplyr::n_distinct(.data$id),
          n_events = sum(.data$cause == 1, na.rm = TRUE),
          n_competing = sum(.data$cause == 2, na.rm = TRUE),
          n_censoring = sum(.data$cause == 0, na.rm = TRUE),
          group_max_followup = max(.data$time, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # Fallback when original_data not available
      group_event_counts <- object$mcc_final |>
        dplyr::group_by(!!rlang::sym(object$by_group)) |>
        dplyr::summarise(
          n_participants = NA_integer_,
          n_events = NA_integer_,
          n_competing = NA_integer_,
          n_censoring = NA_integer_,
          group_max_followup = max(.data$time, na.rm = TRUE),
          .groups = "drop"
        )
    }

    summary_stats <- object$mcc_final |>
      dplyr::group_by(!!rlang::sym(object$by_group)) |>
      dplyr::summarise(
        min_time = min(.data$time, na.rm = TRUE),
        time_of_max_mcc = .data$time[which.max(.data[[mcc_col]])],
        time_to_mcc_one = get_time_to_mcc(
          dplyr::pick(dplyr::everything()),
          mcc_col
        ),
        mcc_at_end_followup = get_mcc_at_end_followup(
          dplyr::pick(dplyr::everything()),
          mcc_col,
          overall_max_followup_time
        ),
        .groups = "drop"
      ) |>
      dplyr::left_join(group_event_counts, by = object$by_group) |>
      dplyr::mutate(
        overall_max_followup = overall_max_followup_time
      )
  } else {
    # Ungrouped summary
    max_followup_time <- get_max_followup_time(object)
    event_counts <- get_event_counts(object)
    max_mcc_idx <- which.max(object$mcc_final[[mcc_col]])

    summary_stats <- list(
      min_time = min(object$mcc_final$time, na.rm = TRUE),
      time_of_max_mcc = object$mcc_final$time[max_mcc_idx],
      max_followup_time = max_followup_time,
      time_to_mcc_one = get_time_to_mcc(object$mcc_final, mcc_col),
      mcc_at_end_followup = get_mcc_at_end_followup(
        object$mcc_final,
        mcc_col,
        max_followup_time
      ),
      n_participants = event_counts$n_participants,
      n_events = event_counts$n_events,
      n_competing = event_counts$n_competing,
      n_censoring = event_counts$n_censoring
    )
  }

  # Create summary object
  result <- list(
    object = object,
    summary_stats = summary_stats,
    method = object$method,
    weighted = object$weighted,
    grouped = is_grouped(object),
    by_group = object$by_group
  )

  class(result) <- "summary.mcc"
  return(result)
}

#' Print method for `mcc` summary objects
#'
#' @param x A `summary.mcc` object
#' @param ... Additional arguments (currently unused)
#'
#' @returns x invisibly
#' @export
print.summary.mcc <- function(x, ...) {
  cli::cli_h1("Summary of Mean Cumulative Count Results")

  # Method information
  method_label <- switch(
    x$method,
    "equation" = "Dong-Yasui Equation Method",
    "sci" = "Sum of Cumulative Incidence Method"
  )
  cli::cli_alert_info("Method: {.val {noquote(method_label)}}")

  if (x$weighted) {
    cli::cli_alert_info("Weighted estimation: {.val {noquote('Yes')}}")
  }

  # Total number of participants and overall observation period
  if (x$grouped) {
    if (!is.na(x$summary_stats$n_participants[1])) {
      total_n <- sum(x$summary_stats$n_participants, na.rm = TRUE)
      cli::cli_alert_info("Total participants: {.val {total_n}}")
      # Show overall observation period for the entire study
      overall_min <- min(x$summary_stats$min_time, na.rm = TRUE)
      overall_max <- x$summary_stats$overall_max_followup[1] # Should be the same for all groups
      cli::cli_alert_info(
        "Overall observation period: [{.val {overall_min}}, {.val {overall_max}}]"
      )
    }
  } else {
    if (!is.na(x$summary_stats$n_participants)) {
      cli::cli_alert_info(
        "Total participants: {.val {x$summary_stats$n_participants}}"
      )
    }
  }

  # Summary statistics
  if (x$grouped) {
    cli::cli_h2("Summary by Group ({.val {noquote(x$by_group)}})")

    # Iterate through each group and print individual summaries
    for (i in seq_len(nrow(x$summary_stats))) {
      group_name <- x$summary_stats[[x$by_group]][i]
      group_stats <- x$summary_stats[i, ]

      cli::cli_h3("Group: {.val {group_name}}")

      # Group-specific participant count
      if (!is.na(group_stats$n_participants)) {
        cli::cli_text(
          "Participants in group: {.val {group_stats$n_participants}}"
        )
      }

      # Group-specific observation period
      cli::cli_text(
        "Group observation period: [{.val {group_stats$min_time}}, {.val {group_stats$group_max_followup}}]"
      )

      if (!is.na(group_stats$time_to_mcc_one)) {
        cli::cli_text("Time to MCC = 1.0: {.val {group_stats$time_to_mcc_one}}")
      } else {
        cli::cli_text("Time to MCC = 1.0: {.val {NA_real_}}")
      }

      cli::cli_text("Time to maximum MCC: {.val {group_stats$time_of_max_mcc}}")
      cli::cli_text(
        "MCC at end of follow-up: {.val {round5(group_stats$mcc_at_end_followup, 4)}}"
      )

      # Add event counts if available (excluding participant count since it's shown above)
      if (!is.na(group_stats$n_events)) {
        cli::cli_text("Events of interest: {.val {group_stats$n_events}}")
        cli::cli_text("Competing risk events: {.val {group_stats$n_competing}}")
        cli::cli_text("Censoring events: {.val {group_stats$n_censoring}}")
      }

      # Add spacing between groups (except for the last one)
      if (i < nrow(x$summary_stats)) {
        cli::cli_text("")
      }
    }
  } else {
    cli::cli_h2("Summary Statistics")
    cli::cli_text(
      "Observation period: [{.val {x$summary_stats$min_time}}, {.val {x$summary_stats$max_followup_time}}]"
    )

    if (!is.na(x$summary_stats$time_to_mcc_one)) {
      cli::cli_text(
        "Time to MCC = 1.0: {.val {x$summary_stats$time_to_mcc_one}}"
      )
    } else {
      cli::cli_text("Time to MCC = 1.0: {.val {NA_real_}}")
    }

    cli::cli_text(
      "Time to maximum MCC: {.val {x$summary_stats$time_of_max_mcc}}"
    )
    cli::cli_text(
      "MCC at end of follow-up: {.val {round5(x$summary_stats$mcc_at_end_followup, 4)}}"
    )

    # Add event counts if available (participant count already shown at top)
    if (!is.na(x$summary_stats$n_events)) {
      cli::cli_h3("Event Count Composition")
      cli::cli_text("Events of interest: {.val {x$summary_stats$n_events}}")
      cli::cli_text(
        "Competing risk events: {.val {x$summary_stats$n_competing}}"
      )
      cli::cli_text("Censoring events: {.val {x$summary_stats$n_censoring}}")
    }
  }

  invisible(x)
}

#' Extract MCC estimates from `mcc` objects
#'
#' @param x An `mcc` object
#' @param ... Additional arguments (currently unused)
#'
#' @returns A tibble with MCC estimates
#' @export
#'
#' @examples
#' # Create sample data
#' library(dplyr)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
#'  ) |>
#'   arrange(id, time)
#'
#' # Calculate MCC
#' mcc_result <- mcc(df, "id", "time", "cause")
#'
#' # Extract MCC estimates
#' estimates <- mcc_estimates(mcc_result)
#' print(estimates)
#'
#' # For grouped analysis
#' df_grouped <- df |>
#'   mutate(group = c("A", "A", "B", "B", "B", "B", "B", "A", "A"))
#'
#' mcc_grouped <- mcc(df_grouped, "id", "time", "cause", by = "group")
#' estimates_grouped <- mcc_estimates(mcc_grouped)
#' print(estimates_grouped)
#'
#' # Clean up
#' rm(df, df_grouped, mcc_result, mcc_grouped, estimates, estimates_grouped)
#'
mcc_estimates <- function(x, ...) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  return(x$mcc_final)
}

#' Extract calculation details from `mcc` objects
#'
#' @param x An `mcc` object
#' @param ... Additional arguments (currently unused)
#'
#' @returns A tibble with calculation details, or NULL if not available
#' @export
#'
#' @examples
#' # Create sample data
#' library(dplyr)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
#'  ) |>
#'   arrange(id, time)
#'
#' # Calculate MCC with details
#' mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
#' mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
#'
#' # Extract calculation details
#' details_eq <- mcc_details(mcc_eq)   # Returns mcc_table
#' details_sci <- mcc_details(mcc_sci) # Returns sci_table
#'
#' print(details_eq)
#' print(details_sci)
#'
#' # Clean up
#' rm(df, mcc_eq, mcc_sci, details_eq, details_sci)
#'
mcc_details <- function(x, ...) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  if (inherits(x, "mcc_equation")) {
    return(x$mcc_table)
  } else if (inherits(x, "mcc_sci")) {
    return(x$sci_table)
  }

  return(NULL)
}
