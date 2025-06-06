#' Plot MCC results
#'
#' @description
#' Creates plots for Mean Cumulative Count (MCC) results. The plotting method
#' automatically adapts based on the `mcc` object class and whether the analysis
#' was grouped.
#'
#' @param x An `mcc` object
#' @param type Character string specifying plot type. Options are:
#'   - "mcc" (default): Plot MCC estimates over time
#'   - "details": Plot calculation details (method-specific)
#' @param groups Character vector specifying which groups to include in grouped analyses.
#'   If NULL (default), all groups are included
#' @param conf_int Logical indicating whether to include confidence intervals if available
#' @param colors Character vector of colors to use for groups. If NULL, uses default colors
#' @param title Character string for plot title. If NULL, generates automatic title
#' @param subtitle Character string for plot subtitle. If NULL, generates automatic subtitle
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @returns A ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2),
#'   group = c("A", "A", "B", "B", "B", "B", "A", "A")
#' )
#'
#' # Calculate MCC
#' mcc_result <- mcc(df, "id", "time", "cause")
#'
#' # Plot MCC over time
#' plot(mcc_result)
#'
#' # Plot calculation details
#' plot(mcc_result, type = "details")
#'
#' # Grouped analysis
#' mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#' plot(mcc_grouped)
#' }
#'
#' @export
#'
#' @examples
#' # Create sample data
#' library(dplyr)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2),
#'   group = c("A", "A", "B", "B", "B", "B", "A", "A")
#' ) |>
#'   arrange(id, time)
#'
#' # Basic MCC plot (ungrouped)
#' mcc_result <- mcc(df, "id", "time", "cause")
#' plot(mcc_result)
#'
#' # Plot calculation details
#' plot(mcc_result, type = "details")
#'
#' # Grouped analysis with custom colors
#' mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#' plot(mcc_grouped)
#'
#' # Customize the grouped plot
#' plot(mcc_grouped,
#'      colors = c("red", "blue"),
#'      title = "MCC by Treatment Group",
#'      subtitle = "Comparison of Event Burden")
#'
#' # Plot details for grouped analysis
#' plot(mcc_grouped, type = "details")
#'
#' # Plot only specific groups
#' plot(mcc_grouped, groups = c("A"))
#'
#' # Compare different methods
#' mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
#' plot(mcc_sci)
#' plot(mcc_sci, type = "details")
#'
#' # Clean up
#' rm(df, mcc_result, mcc_grouped, mcc_sci)
#'
plot.mcc <- function(
  x,
  type = c("mcc", "details"),
  groups = NULL,
  conf_int = FALSE,
  colors = NULL,
  title = NULL,
  subtitle = NULL,
  ...
) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ggplot2} is required for plotting {.cls mcc} objects",
      "i" = "Install it with: {.code install.packages('ggplot2')}"
    ))
  }

  type <- match.arg(type)

  # Dispatch to appropriate plotting method
  if (type == "mcc") {
    plot_mcc_estimates(x, groups, conf_int, colors, title, subtitle, ...)
  } else if (type == "details") {
    plot_mcc_details(x, groups, colors, title, subtitle, ...)
  }
}

#' Plot MCC estimates over time
#'
#' @inheritParams plot.mcc
#' @keywords internal
#' @noRd
plot_mcc_estimates <- function(
  x,
  groups,
  conf_int,
  colors,
  title,
  subtitle,
  ...
) {
  # Get MCC column name based on method
  mcc_col <- if (x$method == "equation") "mcc" else "SumCIs"

  # Prepare data for plotting
  plot_data <- x$mcc_final

  # Filter groups if specified
  if (!is.null(groups) && inherits(x, "mcc_grouped")) {
    if (!all(groups %in% plot_data[[x$by_group]])) {
      missing_groups <- setdiff(groups, plot_data[[x$by_group]])
      cli::cli_warn("Groups not found in data: {missing_groups}")
    }
    plot_data <- plot_data[plot_data[[x$by_group]] %in% groups, ]
  }

  # Create base plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$time, y = .data[[mcc_col]])
  )

  # Add lines/points based on grouping
  if (inherits(x, "mcc_grouped")) {
    p <- p +
      ggplot2::geom_step(
        ggplot2::aes(color = .data[[x$by_group]]),
        linewidth = 1
      )

    # Custom colors if provided
    if (!is.null(colors)) {
      p <- p + ggplot2::scale_color_manual(values = colors)
    }

    # Legend
    p <- p + ggplot2::labs(color = x$by_group)
  } else {
    color <- if (!is.null(colors)) colors[1] else "#2E86AB"
    p <- p +
      ggplot2::geom_step(color = color, linewidth = 1)
  }

  # Add confidence intervals if requested and available
  if (conf_int) {
    # Check if confidence interval data is available
    ci_cols <- grep("^(lower|upper|ci_)", names(plot_data), value = TRUE)
    if (length(ci_cols) >= 2) {
      # Try to identify lower and upper CI columns
      lower_col <- grep("(lower|ci_lower|ci_low)", ci_cols, value = TRUE)[1]
      upper_col <- grep("(upper|ci_upper|ci_high)", ci_cols, value = TRUE)[1]

      if (!is.na(lower_col) && !is.na(upper_col)) {
        if (inherits(x, "mcc_grouped")) {
          p <- p +
            ggplot2::geom_ribbon(
              ggplot2::aes(
                ymin = .data[[lower_col]],
                ymax = .data[[upper_col]],
                fill = .data[[x$by_group]]
              ),
              alpha = 0.2
            )
        } else {
          fill_color <- if (!is.null(colors)) colors[1] else "#2E86AB"
          p <- p +
            ggplot2::geom_ribbon(
              ggplot2::aes(
                ymin = .data[[lower_col]],
                ymax = .data[[upper_col]]
              ),
              fill = fill_color,
              alpha = 0.2
            )
        }
      }
    } else {
      cli::cli_warn(
        "Confidence interval data not available in {.cls mcc} object"
      )
    }
  }

  # Labels and theme
  y_label <- if (x$method == "equation") "Mean Cumulative Count" else
    "Sum of Cumulative Incidences"

  p <- p +
    ggplot2::labs(
      x = "Time",
      y = y_label,
      title = title %||% "Mean Cumulative Count Over Time",
      subtitle = subtitle %||% create_subtitle(x)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10)
    )

  return(p)
}

#' Plot MCC calculation details
#'
#' @inheritParams plot.mcc
#' @keywords internal
#' @noRd
plot_mcc_details <- function(x, groups, colors, title, subtitle, ...) {
  if (x$method == "equation") {
    plot_equation_details(x, groups, colors, title, subtitle, ...)
  } else if (x$method == "sci") {
    plot_sci_details(x, groups, colors, title, subtitle, ...)
  }
}

#' Plot details for equation method
#'
#' @inheritParams plot.mcc
#' @keywords internal
#' @noRd
plot_equation_details <- function(x, groups, colors, title, subtitle, ...) {
  # Check if detailed data is available
  if (is.null(x$mcc_table)) {
    cli::cli_abort(c(
      "Calculation details not available",
      "i" = "Run {.code mcc()} with {.code include_details = TRUE} to include calculation details"
    ))
  }

  plot_data <- x$mcc_table

  # Filter groups if specified
  if (!is.null(groups) && inherits(x, "mcc_grouped")) {
    plot_data <- plot_data[plot_data[[x$by_group]] %in% groups, ]
  }

  # Create a multi-panel plot showing key components
  if (inherits(x, "mcc_grouped")) {
    # Grouped analysis: show average events and survival by group
    p1 <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$time,
        y = .data$ave_events,
        color = .data[[x$by_group]]
      )
    ) +
      ggplot2::geom_step(linewidth = 1) +
      ggplot2::labs(
        title = "Average Events per Time Point",
        y = "Average Events",
        color = x$by_group
      ) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$time,
        y = .data$overall_surv_previous,
        color = .data[[x$by_group]]
      )
    ) +
      ggplot2::geom_step(linewidth = 1) +
      ggplot2::labs(
        title = "Overall Survival",
        y = "Survival Probability",
        color = x$by_group
      ) +
      ggplot2::theme_minimal()
  } else {
    # Ungrouped analysis
    color <- if (!is.null(colors)) colors[1] else "#2E86AB"

    p1 <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$time, y = .data$ave_events)
    ) +
      ggplot2::geom_step(color = color, linewidth = 1) +
      ggplot2::labs(
        title = "Average Events per Time Point",
        y = "Average Events"
      ) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$time, y = .data$overall_surv_previous)
    ) +
      ggplot2::geom_step(color = color, linewidth = 1) +
      ggplot2::labs(title = "Overall Survival", y = "Survival Probability") +
      ggplot2::theme_minimal()
  }

  # Combine plots if patchwork is available
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- p1 /
      p2 +
      patchwork::plot_annotation(
        title = title %||% "MCC Calculation Details (Equation Method)",
        subtitle = subtitle %||% create_subtitle(x)
      )
    return(combined_plot)
  } else {
    # If patchwork not available, return the first plot with a message
    cli::cli_inform(
      "Install {.pkg patchwork} package to see combined detail plots"
    )
    return(p1)
  }
}

#' Plot details for SCI method
#'
#' @inheritParams plot.mcc
#' @keywords internal
#' @noRd
plot_sci_details <- function(x, groups, colors, title, subtitle, ...) {
  # Check if detailed data is available
  if (is.null(x$sci_table)) {
    cli::cli_abort(c(
      "Calculation details not available",
      "i" = "Run {.code mcc()} with {.code include_details = TRUE} to include calculation details"
    ))
  }

  plot_data <- x$sci_table

  # Filter groups if specified
  if (!is.null(groups) && inherits(x, "mcc_grouped")) {
    plot_data <- plot_data[plot_data[[x$by_group]] %in% groups, ]
  }

  # Find CI columns (CI1, CI2, etc.)
  ci_cols <- grep("^CI\\d+$", names(plot_data), value = TRUE)

  if (length(ci_cols) == 0) {
    cli::cli_abort(
      "No cumulative incidence columns found in calculation details"
    )
  }

  # Reshape data for plotting individual CIs
  plot_data_long <- plot_data |>
    tidyr::pivot_longer(
      cols = tidyr::all_of(ci_cols),
      names_to = "event_number",
      values_to = "cumulative_incidence"
    ) |>
    dplyr::mutate(
      event_number = as.numeric(stringr::str_extract(
        .data$event_number,
        "\\d+"
      ))
    )

  # Create plot showing individual cumulative incidences
  if (inherits(x, "mcc_grouped")) {
    p <- ggplot2::ggplot(
      plot_data_long,
      ggplot2::aes(x = .data$time, y = .data$cumulative_incidence)
    ) +
      ggplot2::geom_step(
        ggplot2::aes(
          color = factor(.data$event_number),
          linetype = .data[[x$by_group]]
        ),
        linewidth = 1
      ) +
      ggplot2::labs(
        color = "Event Number",
        linetype = x$by_group,
        y = "Cumulative Incidence"
      )
  } else {
    p <- ggplot2::ggplot(
      plot_data_long,
      ggplot2::aes(
        x = .data$time,
        y = .data$cumulative_incidence,
        color = factor(.data$event_number)
      )
    ) +
      ggplot2::geom_step(linewidth = 1) +
      ggplot2::labs(color = "Event Number", y = "Cumulative Incidence")
  }

  # Add the sum of CIs
  if (inherits(x, "mcc_grouped")) {
    p <- p +
      ggplot2::geom_step(
        data = plot_data,
        ggplot2::aes(
          x = .data$time,
          y = .data$SumCIs,
          color = "Sum",
          linetype = .data[[x$by_group]]
        ),
        linewidth = 1.5
      )
  } else {
    p <- p +
      ggplot2::geom_step(
        data = plot_data,
        ggplot2::aes(x = .data$time, y = .data$SumCIs, color = "Sum"),
        linewidth = 1.5
      )
  }

  # Styling
  p <- p +
    ggplot2::labs(
      x = "Time",
      title = title %||% "Cumulative Incidence Components (SCI Method)",
      subtitle = subtitle %||% create_subtitle(x)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10)
    )

  return(p)
}

#' Create automatic subtitle for plots
#'
#' @param x `mcc` object
#' @returns Character string for subtitle
#' @keywords internal
#' @noRd
create_subtitle <- function(x) {
  method_label <- switch(
    x$method,
    "equation" = "Dong-Yasui Equation Method",
    "sci" = "Sum of Cumulative Incidence Method"
  )

  subtitle_parts <- method_label

  if (x$weighted) {
    subtitle_parts <- paste(subtitle_parts, "(Weighted)")
  }

  if (inherits(x, "mcc_grouped")) {
    n_groups <- length(unique(x$mcc_final[[x$by_group]]))
    subtitle_parts <- paste0(subtitle_parts, " - ", n_groups, " groups")
  }

  return(subtitle_parts)
}

#' Auto-plot method for `mcc` objects
#'
#' @description
#' Convenience function that automatically creates an appropriate plot
#' for `mcc` objects. This is called when using the base R `plot()` function.
#'
#' @param x An `mcc` object
#' @param ... Additional arguments passed to plot.mcc
#'
#' @returns A ggplot2 object
#'
#' @examples
#' # Create sample data
#' library(dplyr)
#' library(ggplot2)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3),
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2),
#'   treatment = c("Control", "Control", "Treatment", "Treatment",
#'                 "Treatment", "Treatment", "Control", "Control")
#' ) |>
#'   arrange(id, time)
#'
#' # Calculate MCC
#' mcc_result <- mcc(df, "id", "time", "cause", by = "treatment")
#'
#' # Use autoplot (ggplot2 style)
#' p <- autoplot(mcc_result)
#' print(p)
#'
#' # Customize with ggplot2 functions
#' p_custom <- autoplot(mcc_result) +
#'   theme_classic() +
#'   labs(caption = "Data from hypothetical study") +
#'   geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5)
#'
#' print(p_custom)
#'
#' # Clean up
#' rm(df, mcc_result, p, p_custom)
#'
autoplot.mcc <- function(x, ...) {
  plot.mcc(x, ...)
}

# nocov start
register_all_s3_methods <- function() {
  s3_register("ggplot2::autoplot", "mcc")
}
# nocov end
