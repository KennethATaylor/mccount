test_that("plot.mcc works with different configurations", {
  skip_if_not_installed("ggplot2")

  # Create test data
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0),
    group = c("A", "A", "B", "B", "B", "B", "B")
  )

  # Test ungrouped plots
  mcc_simple <- mcc(df, "id", "time", "cause")

  p1 <- plot(mcc_simple)
  expect_s3_class(p1, "ggplot")

  # Test grouped plots
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  p3 <- plot(mcc_grouped)
  expect_s3_class(p3, "ggplot")

  p4 <- plot(mcc_grouped, groups = "A")
  expect_s3_class(p4, "ggplot")

  # Test custom colors and titles
  p5 <- plot(
    mcc_grouped,
    colors = c("red", "blue"),
    title = "Custom Title",
    subtitle = "Custom Subtitle"
  )
  expect_s3_class(p5, "ggplot")
})

test_that("plot.mcc error handling works correctly", {
  # Test with include_details = FALSE
  df <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(7, 8, 1, 5),
    cause = c(1, 0, 0, 2)
  )

  mcc_no_details <- mcc(
    df,
    "id",
    "time",
    "cause",
    method = "sci",
    include_details = FALSE
  )

  expect_error(
    plot(mcc_no_details, type = "components"),
    "Calculation components not available"
  )
})

test_that("autoplot.mcc works correctly", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(4, 8, 1, 5),
    cause = c(1, 0, 0, 2)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  p1 <- autoplot(mcc_result)
  p2 <- plot(mcc_result)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # Should produce equivalent plots
  expect_equal(class(p1), class(p2))
})

test_that("plot.mcc throws error when type='components' with equation method", {
  # Create sample data
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
  )

  # Calculate MCC using equation method
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")

  # Test that requesting components plot throws appropriate error
  expect_error(
    plot(mcc_eq, type = "components"),
    class = "rlang_error"
  )

  # Test the specific error message content
  expect_error(
    plot(mcc_eq, type = "components"),
    "Details plots are only available for.*method = \"sci\"",
    fixed = FALSE # Use regex matching
  )

  # Test that the error message contains helpful guidance
  expect_error(
    plot(mcc_eq, type = "components"),
    "Use.*type = \"mcc\".*to plot the MCC estimates",
    fixed = FALSE
  )

  # Test that the error message mentions the alternative
  expect_error(
    plot(mcc_eq, type = "components"),
    "method = \"sci\".*for components plots",
    fixed = FALSE
  )
})

test_that("plot.mcc works correctly with type='components' for SCI method", {
  # Create sample data
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
  )

  # Calculate MCC using SCI method
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")

  # Test that components plot works for SCI method
  expect_no_error(
    plot(mcc_sci, type = "components")
  )

  # Test that it returns a ggplot object
  p <- plot(mcc_sci, type = "components")
  expect_s3_class(p, "ggplot")
})

test_that("plot.mcc type argument matching works correctly", {
  # Create sample data
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
  )

  # Test with equation method
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")

  # Test that type="mcc" works (default)
  expect_no_error(plot(mcc_eq, type = "mcc"))
  expect_no_error(plot(mcc_eq)) # Default should be "mcc"

  # Test that invalid type throws error
  expect_error(
    plot(mcc_eq, type = "invalid"),
    "should be one of"
  )

  # Test with SCI method
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")

  # Test that both valid types work
  expect_no_error(plot(mcc_sci, type = "mcc"))
  expect_no_error(plot(mcc_sci, type = "components"))
  expect_no_error(plot(mcc_sci)) # Default should be "mcc"
})


test_that("plot.mcc handles group filtering correctly", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5, 6),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4, 9),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0, 2), # ID 4, 5, 6 end with cause = 0 or 2
    group = c("A", "A", "B", "B", "B", "B", "B", "C", "C", "C")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test plotting specific groups
  expect_no_error(p_filtered <- plot(mcc_grouped, groups = c("A", "B")))
  expect_s3_class(p_filtered, "ggplot")

  # Test warning for non-existent groups
  expect_warning(
    plot(mcc_grouped, groups = c("A", "NonExistent")),
    "Groups not found in data"
  )
})

test_that("plot.mcc color customization works", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0), # ID 4 and 5 end with cause = 0
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  # Test ungrouped with custom colors
  mcc_simple <- mcc(df, "id", "time", "cause")
  expect_no_error(p_color <- plot(mcc_simple, colors = "red"))

  # Test grouped with custom colors
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  expect_no_error(p_colors <- plot(mcc_grouped, colors = c("red", "blue")))

  expect_s3_class(p_color, "ggplot")
  expect_s3_class(p_colors, "ggplot")
})


test_that("geom_line_mcc works with ungrouped mcc objects", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test that geom_line_mcc returns ggplot layers
  layers <- geom_line_mcc(mcc_result)
  expect_type(layers, "list")

  # Test that it can be added to a plot
  expect_no_error({
    p <- plot(mcc_result) + geom_line_mcc(mcc_result)
    print(p)
  })
})

test_that("geom_line_mcc works with grouped mcc objects", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0), # ID 4 and 5 end with cause = 0
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test with grouped data
  layers <- geom_line_mcc(mcc_grouped)
  expect_type(layers, "list")

  # Test that it can be added to grouped plot
  expect_no_error({
    p <- plot(mcc_grouped) + geom_line_mcc(mcc_grouped)
    print(p)
  })
})

test_that("geom_line_mcc handles different threshold values", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test different threshold values
  expect_no_error({
    layers_default <- geom_line_mcc(mcc_result) # threshold = 1.0
    layers_custom <- geom_line_mcc(mcc_result, threshold = 0.5)
  })

  expect_type(geom_line_mcc(mcc_result, threshold = 2.0), "list")
})

test_that("geom_line_mcc handles custom styling arguments", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test custom styling options
  expect_no_error({
    layers <- geom_line_mcc(
      mcc_result,
      linetype = "dotted",
      color = "red",
      alpha = 0.5,
      linewidth = 1.0,
      show_labels = TRUE,
      label_size = 4,
      label_nudge_x = 0.1,
      label_nudge_y = 0.1
    )
  })

  expect_type(layers, "list")
})

test_that("geom_line_mcc handles cases where MCC never reaches threshold", {
  skip_if_not_installed("ggplot2")

  # Create data where MCC likely won't reach high threshold
  # Single participant with only censoring - no events
  df <- data.frame(
    id = 1,
    time = 5,
    cause = 0 # No events, MCC stays at 0, already ends with cause = 0
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Should inform user when threshold is never reached
  expect_message(
    geom_line_mcc(mcc_result, threshold = 10),
    "MCC never reaches 10"
  )
})

test_that("geom_line_mcc validates input arguments", {
  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test error for non-mcc object
  expect_error(
    geom_line_mcc(df),
    "must be an.*mcc.*object"
  )

  # Test error for invalid input types
  expect_error(
    geom_line_mcc("not_mcc"),
    "must be an.*mcc.*object"
  )
})

test_that("geom_line_mcc works with different methods (equation vs sci)", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")

  # Both methods should work
  expect_no_error({
    layers_eq <- geom_line_mcc(mcc_eq)
    layers_sci <- geom_line_mcc(mcc_sci)
  })

  expect_type(layers_eq, "list")
  expect_type(layers_sci, "list")
})

test_that("plot.mcc uses correct data table for each method", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0) # ID 4 and 5 end with cause = 0
  )

  # Test equation method uses mcc_table
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
  expect_no_error(p_eq <- plot(mcc_eq))
  expect_s3_class(p_eq, "ggplot")

  # Test SCI method uses sci_table
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
  expect_no_error(p_sci <- plot(mcc_sci))
  expect_s3_class(p_sci, "ggplot")
})

test_that("plot.mcc handles grouped data correctly with both methods", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0), # ID 4 and 5 end with cause = 0
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  # Test grouped equation method
  mcc_eq_grouped <- mcc(
    df,
    "id",
    "time",
    "cause",
    by = "group",
    method = "equation"
  )
  expect_no_error(p_eq_grouped <- plot(mcc_eq_grouped))
  expect_s3_class(p_eq_grouped, "ggplot")

  # Test grouped SCI method
  mcc_sci_grouped <- mcc(
    df,
    "id",
    "time",
    "cause",
    by = "group",
    method = "sci"
  )
  expect_no_error(p_sci_grouped <- plot(mcc_sci_grouped))
  expect_s3_class(p_sci_grouped, "ggplot")
})

test_that("plot.mcc handles confidence intervals when available", {
  skip_if_not_installed("ggplot2")

  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test with conf_int = TRUE (should warn if not available)
  expect_warning(
    plot(mcc_result, conf_int = TRUE),
    "Confidence interval data not available"
  )
})


test_that("plot.mcc handles group filtering correctly", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5, 6, 6),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3, 4, 5),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2, 1, 0),
    group = c("A", "A", "B", "B", "B", "B", "B", "C", "C", "C", "C")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test plotting specific groups
  expect_no_error(p_filtered <- plot(mcc_grouped, groups = c("A", "B")))
  expect_s3_class(p_filtered, "ggplot")

  # Test warning for non-existent groups
  expect_warning(
    plot(mcc_grouped, groups = c("A", "NonExistent")),
    "Groups not found in data"
  )
})

test_that("plot.mcc color customization works", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  # Test ungrouped with custom colors
  mcc_simple <- mcc(df, "id", "time", "cause")
  expect_no_error(p_color <- plot(mcc_simple, colors = "red"))

  # Test grouped with custom colors
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  expect_no_error(p_colors <- plot(mcc_grouped, colors = c("red", "blue")))

  expect_s3_class(p_color, "ggplot")
  expect_s3_class(p_colors, "ggplot")
})
