#' Convert MCC object to data.frame
#'
#' @description
#' Extracts the MCC estimates from an MCC object and returns them as a
#' standard data.frame. This is useful for further analysis or when working
#' with packages that expect standard data.frame objects.
#'
#' @param x An MCC object
#' @param ... Additional arguments (currently unused)
#'
#' @returns A data.frame with MCC estimates
#' @export
as.data.frame.mcc <- function(x, ...) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  return(as.data.frame(x$mcc_final))
}

#' Get the method used for MCC calculation
#'
#' @param x An MCC object
#'
#' @returns Character string indicating the method ("equation" or "sci")
#' @export
mcc_method <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  return(x$method)
}

#' Check if MCC object uses weighted estimation
#'
#' @param x An MCC object
#'
#' @returns Logical indicating whether weighted estimation was used
#' @export
is_weighted <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  return(x$weighted)
}

#' Check if MCC object is from grouped analysis
#'
#' @param x An MCC object
#'
#' @returns Logical indicating whether the analysis was grouped
#' @export
is_grouped <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  return(inherits(x, "mcc_grouped"))
}

#' Get grouping variable name from grouped MCC object
#'
#' @param x An MCC object
#'
#' @returns Character string with grouping variable name, or NULL if not grouped
#' @export
mcc_grouping_var <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  if (!is_grouped(x)) {
    return(NULL)
  }

  return(x$by_group)
}

#' Extract unique groups from grouped MCC object
#'
#' @param x An MCC object
#'
#' @returns Character vector of unique group values, or NULL if not grouped
#' @export
mcc_groups <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  if (!is_grouped(x)) {
    return(NULL)
  }

  return(unique(x$mcc_final[[x$by_group]]))
}

#' Subset MCC object by groups
#'
#' @description
#' For grouped MCC objects, extracts results for specified groups only.
#' This is useful for focusing on specific groups of interest or creating
#' custom visualizations.
#'
#' @param x A grouped MCC object
#' @param groups Character vector of group names to include
#'
#' @returns An MCC object containing only the specified groups
#' @export
subset_mcc <- function(x, groups) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls mcc} object")
  }

  if (!is_grouped(x)) {
    cli::cli_abort("{.arg x} must be a grouped {.cls mcc} object")
  }

  if (!is.character(groups)) {
    cli::cli_abort("{.arg groups} must be a character vector")
  }

  # Check if all requested groups exist
  available_groups <- mcc_groups(x)
  missing_groups <- setdiff(groups, available_groups)

  if (length(missing_groups) > 0) {
    cli::cli_abort(c(
      "Groups not found in {.cls mcc} object:",
      "x" = "Missing: {missing_groups}",
      "i" = "Available groups: {available_groups}"
    ))
  }

  # Create a copy of the object
  result <- x

  # Filter main results
  result$mcc_final <- x$mcc_final[x$mcc_final[[x$by_group]] %in% groups, ]

  # Filter detailed tables if they exist
  detail_tables <- c(
    "mcc_table",
    "sci_table",
    "mcc_base",
    "original_data",
    "adjusted_data"
  )

  for (table_name in detail_tables) {
    if (table_name %in% names(x) && !is.null(x[[table_name]])) {
      if (x$by_group %in% names(x[[table_name]])) {
        result[[table_name]] <- x[[table_name]][
          x[[table_name]][[x$by_group]] %in% groups,
        ]
      }
    }
  }

  # Filter all_cis if it exists (for SCI method)
  if ("all_cis" %in% names(x) && !is.null(x$all_cis)) {
    result$all_cis <- x$all_cis[names(x$all_cis) %in% groups]
  }

  return(result)
}

#' Get final MCC value for each group
#'
#' @description
#' Extracts the final (maximum time) MCC value for each group in a grouped
#' analysis, or the overall final MCC value for ungrouped analyses.
#'
#' @param x An MCC object
#'
#' @returns A named numeric vector with final MCC values
#' @export
mcc_final_values <- function(x) {
  if (!is_mcc(x)) {
    cli::cli_abort("{.arg x} must be an MCC object")
  }

  mcc_col <- if (x$method == "equation") "mcc" else "SumCIs"

  if (is_grouped(x)) {
    # For grouped analyses, get final value for each group
    final_values <- x$mcc_final |>
      dplyr::group_by(!!rlang::sym(x$by_group)) |>
      dplyr::summarise(
        final_mcc = .data[[mcc_col]][which.max(.data$time)],
        .groups = "drop"
      )

    # Convert to named vector
    result <- final_values$final_mcc
    names(result) <- final_values[[x$by_group]]

    return(result)
  } else {
    # For ungrouped analyses, get the final value
    final_value <- x$mcc_final[[mcc_col]][nrow(x$mcc_final)]
    names(final_value) <- "Overall"

    return(final_value)
  }
}

#' Compare MCC objects
#'
#' @description
#' Compares two MCC objects and returns a summary of differences.
#' Useful for comparing results from different methods or parameter settings.
#'
#' @param x First MCC object
#' @param y Second MCC object
#' @param tolerance Numeric tolerance for comparing MCC values (default: 1e-6)
#'
#' @returns A list summarizing the comparison
#' @export
compare_mcc <- function(x, y, tolerance = 1e-6) {
  if (!is_mcc(x) || !is_mcc(y)) {
    cli::cli_abort("Both {.arg x} and {.arg y} must be {.cls mcc} objects")
  }

  # Basic comparison
  comparison <- list(
    methods_match = x$method == y$method,
    weighted_match = x$weighted == y$weighted,
    grouped_match = is_grouped(x) == is_grouped(y),
    same_grouping_var = identical(x$by_group, y$by_group)
  )

  # Compare final values
  final_x <- mcc_final_values(x)
  final_y <- mcc_final_values(y)

  if (
    length(final_x) == length(final_y) && all(names(final_x) == names(final_y))
  ) {
    comparison$final_values_close <- all(abs(final_x - final_y) <= tolerance)
    comparison$max_difference <- max(abs(final_x - final_y))
  } else {
    comparison$final_values_close <- FALSE
    comparison$max_difference <- NA
  }

  # Overall assessment
  comparison$objects_equivalent <- all(c(
    comparison$methods_match,
    comparison$weighted_match,
    comparison$grouped_match,
    comparison$same_grouping_var,
    comparison$final_values_close %||% FALSE
  ))

  class(comparison) <- "mcc_comparison"
  return(comparison)
}

#' Print method for MCC comparison objects
#'
#' @param x An mcc_comparison object
#' @param ... Additional arguments (currently unused)
#'
#' @returns x invisibly
#' @export
print.mcc_comparison <- function(x, ...) {
  cli::cli_h1("MCC Object Comparison")

  if (x$objects_equivalent) {
    cli::cli_alert_success("Objects are equivalent within tolerance")
  } else {
    cli::cli_alert_warning("Objects differ in one or more aspects")
  }

  cli::cli_h2("Comparison Details")

  status_icon <- function(x)
    if (x) cli::col_green(cli::symbol$tick) else cli::col_red(cli::symbol$cross)

  cli::cli_text("{status_icon(x$methods_match)} Methods match")
  cli::cli_text("{status_icon(x$weighted_match)} Weighted status matches")
  cli::cli_text("{status_icon(x$grouped_match)} Grouping status matches")
  cli::cli_text("{status_icon(x$same_grouping_var)} Grouping variables match")
  cli::cli_text(
    "{status_icon(x$final_values_close %||% FALSE)} Final values are close"
  )

  if (!is.na(x$max_difference)) {
    cli::cli_text("Maximum difference in final values: {x$max_difference}")
  }

  invisible(x)
}
