#' Plot MCC results
#'
#' @description
#' Creates plots for Mean Cumulative Count (MCC) results. The plotting method
#' automatically adapts based on the `mcc` object class and whether the analysis
#' was grouped.
#'
#' @param x An `mcc` object
#' @param type Character string specifying plot type:
#'   - "mcc" (default): Plot MCC estimates over time
#'   - "components": Show individual cumulative incidence components (SCI method only)
#' @param groups Character vector specifying which groups to include in grouped analyses.
#'   If NULL (default), all groups are included
#' @param conf_int Logical indicating whether to include confidence intervals if available
#' @param colors Character vector of colors to use for groups. If NULL, uses default colors
#' @param title Character string for plot title. If NULL, generates automatic title
#' @param subtitle Character string for plot subtitle. If NULL, generates automatic subtitle
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @returns A ggplot2 object
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
#' # Plot only specific groups
#' plot(mcc_grouped, groups = c("A"))
#'
#' # Compare different methods - equation method only shows MCC
#' mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
#' plot(mcc_eq)
#'
#' # SCI method can show components of cumulative incidence components
#' mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
#' plot(mcc_sci)  # Shows main MCC plot
#' plot(mcc_sci, type = "components")  # Shows CI components
#'
#' # Clean up
#' rm(df, mcc_result, mcc_grouped, mcc_eq, mcc_sci)
#'
plot.mcc <- function(
  x,
  type = c("mcc", "components"),
  groups = NULL,
  conf_int = FALSE,
  colors = NULL,
  title = NULL,
  subtitle = NULL,
  ...
) {
  # Check if ggplot2 is available
  rlang::check_installed("ggplot2", reason = "for plotting MCC objects")

  type <- match.arg(type)

  # Check if components plot is requested for equation method
  if (type == "components" && x$method == "equation") {
    cli::cli_abort(c(
      "Details plots are only available for {.code method = \"sci\"}",
      "i" = "Use {.code type = \"mcc\"} to plot the MCC estimates, or use {.code method = \"sci\"} for components plots"
    ))
  }

  # Dispatch to appropriate plotting method
  if (type == "mcc") {
    plot_mcc_estimates(x, groups, conf_int, colors, title, subtitle, ...)
  } else if (type == "components") {
    # Only SCI method reaches here due to check above
    plot_mcc_components(x, groups, colors, title, subtitle, ...)
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

  # Get the appropriate data table based on method
  # Use the detailed data tables that contain all time points, not just mcc_final
  if (x$method == "equation") {
    plot_data <- x$mcc_table
  } else {
    # For SCI method, use sci_table which contains all time points
    plot_data <- x$sci_table
  }

  # Filter groups if specified
  if (!is.null(groups) && is_grouped(x)) {
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
  if (is_grouped(x)) {
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
        if (is_grouped(x)) {
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

#' Plot MCC calculation components
#'
#' @inheritParams plot.mcc
#' @keywords internal
#' @noRd
plot_mcc_components <- function(x, groups, colors, title, subtitle, ...) {
  # Check if detailed data is available
  if (is.null(x$sci_table)) {
    cli::cli_abort(c(
      "Calculation components not available",
      "i" = "Run {.code mcc()} with {.code include_details = TRUE} to include calculation components"
    ))
  }

  plot_data <- x$sci_table

  # Filter groups if specified
  if (!is.null(groups) && is_grouped(x)) {
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
  if (is_grouped(x)) {
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
  if (is_grouped(x)) {
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

  if (is_grouped(x)) {
    n_groups <- length(unique(x$mcc_final[[x$by_group]]))
    subtitle_parts <- paste0(subtitle_parts, " - ", n_groups, " groups")
  }

  return(subtitle_parts)
}


#' Add Reference Lines at an MCC Threshold to ggplot2 Objects
#'
#' @description
#' Adds horizontal and vertical reference lines to mark when the Mean Cumulative
#' Count (MCC) reaches the `threshold`. This function returns a list of `ggplot2`
#' geoms that can be added to existing plots using the `+` operator. For
#' grouped analyses, it creates separate reference lines for each group.
#'
#' @param mcc_object An object of class `mcc` containing MCC estimates.
#' @param threshold numeric;determines MCC value threshold to use (default =
#'     `1.0`)
#' @param linetype Line type for the reference lines. Default is `2` (dashed).
#'   Can be numeric (1-6) or character ("solid", "dashed", "dotted", etc.).
#' @param color Color for the reference lines. If `NULL` (default), uses gray.
#' @param alpha Transparency level for the reference lines. Default is `0.7`.
#' @param linewidth Width of the reference lines. Default is `0.5`.
#' @param show_labels Logical indicating whether to add text labels at the
#'   intersection points. Default is `FALSE`.
#' @param label_size Size of the text labels if `show_labels = TRUE`. Default is `3`.
#' @param label_nudge_x Horizontal offset for labels. Default is `0`.
#' @param label_nudge_y Vertical offset for labels. Default is `0.05`.
#'
#' @returns A `ggplot2` layer object that can be added to a ggplot using the `+`
#'     operator.
#'
#' @details
#' This function identifies the time when MCC first reaches or exceeds the
#' specified MCC `threshold`. It then creates:
#'
#' - A horizontal line from x = 0 to the time when MCC = `threshold`
#' - A vertical line from y = 0 to MCC = `threshold` at that time point
#'
#' For grouped analyses, separate reference lines are created for each group
#' that reaches MCC = `threshold`. Groups that never reach MCC = `threshold`
#' will not have reference lines added.
#'
#' The function is designed to work seamlessly with the existing `plot.mcc()`
#' method and can be chained using ggplot2's `+` syntax.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' # Ungrouped analysis
#' mcc_overall <- mcc(df, "id", "time", "cause")
#'
#' # Basic plot with reference lines
#' plot(mcc_overall) +
#'   geom_line_mcc(mcc_overall) +
#'   labs(title = "MCC with Reference Lines at 1.0")
#'
#' # Grouped analysis
#' mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#'
#' # Plot with group-specific reference lines
#' plot(mcc_grouped) +
#'   geom_line_mcc(mcc_grouped, linetype = "dotted", alpha = 0.8) +
#'   labs(title = "Grouped MCC with Reference Lines")
#'
#' # With labels
#' plot(mcc_overall) +
#'   geom_line_mcc(mcc_overall, show_labels = TRUE, color = "red") +
#'   labs(title = "MCC with Labeled Reference Lines")
#'
#' # Clean up
#' rm(df, mcc_overall, mcc_grouped)
#' }
#'
geom_line_mcc <- function(
  mcc_object,
  threshold = 1.0,
  linetype = 2,
  color = NULL,
  alpha = 0.7,
  linewidth = 0.5,
  show_labels = FALSE,
  label_size = 3,
  label_nudge_x = 0,
  label_nudge_y = 0.05
) {
  # Input validation
  if (!is_mcc(mcc_object)) {
    cli::cli_abort("{.arg mcc_object} must be an {.cls mcc} object")
  }

  # Check if ggplot2 is available
  rlang::check_installed(
    "ggplot2",
    reason = "for adding reference lines to plots"
  )

  # Set default color if not provided
  if (is.null(color)) {
    color <- "gray50"
  }

  # Get MCC column name based on method
  mcc_col <- if (mcc_object$method == "equation") "mcc" else "SumCIs"

  # Create a custom layer function that returns multiple geoms
  layer_function <- function() {
    # Initialize list to store annotation data
    segments_data <- data.frame()
    labels_data <- data.frame()

    if (is_grouped(mcc_object)) {
      # Handle grouped analysis
      group_var <- mcc_object$by_group
      groups <- unique(mcc_object$mcc_final[[group_var]])

      for (i in seq_along(groups)) {
        group_data <- mcc_object$mcc_final[
          mcc_object$mcc_final[[group_var]] == groups[i],
        ]

        # Get time when MCC reaches 1.0 for this group
        mcc_1_time <- get_time_to_mcc(
          group_data,
          mcc_col,
          threshold = threshold
        )

        # Only add lines if MCC reaches 1.0
        if (!is.na(mcc_1_time)) {
          # Add vertical line data
          segments_data <- rbind(
            segments_data,
            data.frame(
              x = mcc_1_time,
              y = 0,
              xend = mcc_1_time,
              yend = threshold,
              group = groups[i]
            )
          )

          # Add horizontal line data
          segments_data <- rbind(
            segments_data,
            data.frame(
              x = 0,
              y = threshold,
              xend = mcc_1_time,
              yend = threshold,
              group = groups[i]
            )
          )

          # Add label data if requested
          if (show_labels) {
            labels_data <- rbind(
              labels_data,
              data.frame(
                x = mcc_1_time + label_nudge_x,
                y = threshold + label_nudge_y,
                label = glue::glue("{groups[i]}: t = {round(mcc_1_time, 1)}"),
                group = groups[i]
              )
            )
          }
        }
      }
    } else {
      # Handle ungrouped analysis
      mcc_1_time <- get_time_to_mcc(
        mcc_object$mcc_final,
        mcc_col,
        threshold = threshold
      )

      # Only add lines if MCC reaches 1.0
      if (!is.na(mcc_1_time)) {
        # Add vertical line data
        segments_data <- rbind(
          segments_data,
          data.frame(
            x = mcc_1_time,
            y = 0,
            xend = mcc_1_time,
            yend = threshold
          )
        )

        # Add horizontal line data
        segments_data <- rbind(
          segments_data,
          data.frame(
            x = 0,
            y = threshold,
            xend = mcc_1_time,
            yend = threshold
          )
        )

        # Add label data if requested
        if (show_labels) {
          labels_data <- rbind(
            labels_data,
            data.frame(
              x = mcc_1_time + label_nudge_x,
              y = threshold + label_nudge_y,
              label = glue::glue(
                "MCC = {threshold} at t = {round(mcc_1_time, 1)}"
              )
            )
          )
        }
      } else {
        # Warn user if MCC never reaches threshold
        cli::cli_inform(c(
          "i" = "MCC never reaches {threshold} in the observed follow-up period",
          "i" = "No reference lines added"
        ))
      }
    }

    # Create the layers
    layers <- list()

    # Add segments if we have data
    if (nrow(segments_data) > 0) {
      layers <- append(
        layers,
        list(
          ggplot2::geom_segment(
            data = segments_data,
            ggplot2::aes(
              x = .data$x,
              y = .data$y,
              xend = .data$xend,
              yend = .data$yend
            ),
            linetype = linetype,
            color = color,
            alpha = alpha,
            linewidth = linewidth,
            inherit.aes = FALSE
          )
        )
      )
    }

    # Add labels if we have data
    if (show_labels && nrow(labels_data) > 0) {
      layers <- append(
        layers,
        list(
          ggplot2::geom_text(
            data = labels_data,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
            size = label_size,
            color = color,
            inherit.aes = FALSE
          )
        )
      )
    }

    return(layers)
  }

  # Return the layers
  return(layer_function())
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
