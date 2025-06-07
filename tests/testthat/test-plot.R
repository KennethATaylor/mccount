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
