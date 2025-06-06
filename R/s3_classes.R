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
  if (!inherits(x$mcc_final, "data.frame")) {
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
  if (inherits(x, "mcc_grouped")) {
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

  if (!inherits(x$mcc_final, "data.frame")) {
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
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
#' ) |>
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
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3), # Times will be adjusted for id = 5
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
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
  cli::cli_alert_info("Method: {method_label}")

  if (x$weighted) {
    cli::cli_alert_info("Weighted estimation: Yes")
  }

  # Print grouping information if applicable
  if (inherits(x, "mcc_grouped")) {
    n_groups <- length(unique(x$mcc_final[[x$by_group]]))
    cli::cli_alert_info("Grouped by: {x$by_group} ({n_groups} groups)")
  }

  # Print sample of results
  cli::cli_h2("MCC Estimates")

  if (inherits(x, "mcc_grouped")) {
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
        cli::cli_text("# ... with {nrow(group_data) - 3} more rows")
      }
    }

    if (length(groups) > n_show) {
      cli::cli_text("# ... with {length(groups) - n_show} more groups")
    }
  } else {
    # For ungrouped results
    if (nrow(x$mcc_final) <= 10) {
      print(x$mcc_final)
    } else {
      print(utils::head(x$mcc_final, 6))
      cli::cli_text("# ... with {nrow(x$mcc_final) - 6} more rows")
    }
  }

  # Print call if available
  if (!is.null(x$call)) {
    cli::cli_h2("Call")
    cli::cli_code(deparse(x$call))
  }

  invisible(x)
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
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3), # Times will be adjusted for id = 5
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
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

  if (inherits(object, "mcc_grouped")) {
    # Grouped summary
    summary_stats <- object$mcc_final |>
      dplyr::group_by(!!rlang::sym(object$by_group)) |>
      dplyr::summarise(
        n_timepoints = dplyr::n(),
        min_time = min(.data$time, na.rm = TRUE),
        max_time = max(.data$time, na.rm = TRUE),
        final_mcc = .data[[mcc_col]][which.max(.data$time)],
        .groups = "drop"
      )
  } else {
    # Ungrouped summary
    summary_stats <- list(
      n_timepoints = nrow(object$mcc_final),
      min_time = min(object$mcc_final$time, na.rm = TRUE),
      max_time = max(object$mcc_final$time, na.rm = TRUE),
      final_mcc = object$mcc_final[[mcc_col]][nrow(object$mcc_final)]
    )
  }

  # Create summary object
  result <- list(
    object = object,
    summary_stats = summary_stats,
    method = object$method,
    weighted = object$weighted,
    grouped = inherits(object, "mcc_grouped"),
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
  cli::cli_alert_info("Method: {method_label}")

  if (x$weighted) {
    cli::cli_alert_info("Weighted estimation: Yes")
  }

  # Summary statistics
  if (x$grouped) {
    cli::cli_h2("Summary by Group ({x$by_group})")
    print(x$summary_stats)
  } else {
    cli::cli_h2("Summary Statistics")
    cli::cli_text("Number of time points: {x$summary_stats$n_timepoints}")
    cli::cli_text(
      "Time range: [{x$summary_stats$min_time}, {x$summary_stats$max_time}]"
    )
    cli::cli_text("Final MCC: {round(x$summary_stats$final_mcc, 4)}")
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
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
#' ) |>
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
#'   mutate(group = c("A", "A", "B", "B", "B", "B", "A", "A"))
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
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
#' ) |>
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
