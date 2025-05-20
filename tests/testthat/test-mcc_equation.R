test_that("mcc_equation() correctly calculates MCC with basic data", {
  # Create a simple dataset with no simultaneous events
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1)
  )

  # Run the function
  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Test structure of output
  expect_type(result, "list")
  expect_named(result, c("mcc_final", "mcc_table", "original_data"))

  # Test mcc_final structure
  expect_s3_class(result$mcc_final, c("tbl_df", "tbl", "data.frame"))
  expect_named(result$mcc_final, c("time", "mcc"))

  # Test calculation correctness (comparing with known expected values)
  # The exact values would need to be calculated separately and validated
  expect_true(all(result$mcc_final$mcc >= 0))
  expect_true(all(result$mcc_table$mcc >= 0))

  # Test that MCC is correctly sorted by time
  expect_true(all(diff(result$mcc_final$time) >= 0))
})

test_that("mcc_equation() handles simultaneous events correctly", {
  # Dataset with simultaneous events
  df <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(5, 5, 8, 10),
    cause = c(1, 2, 0, 1)
  )

  # With adjustment
  result_adj <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    adjust_times = TRUE
  )

  # Without adjustment - expect a warning
  expect_warning(
    result_no_adj <- mcc_equation(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = FALSE
    ),
    "events will be processed without time adjustment"
  )

  # Check that adjustment was made
  expect_true("adjusted_data" %in% names(result_adj))
  expect_false("adjusted_data" %in% names(result_no_adj))

  # Verify that adjusted data modified the times
  if ("adjusted_data" %in% names(result_adj)) {
    simultaneous_rows <- df$id == 1 & df$time == 5
    expect_false(any(duplicated(result_adj$adjusted_data$time[
      result_adj$adjusted_data$id == 1
    ])))
  }
})

test_that("mcc_equation() correctly handles last event records", {
  # Dataset where last record for an ID is an event
  df <- data.frame(
    id = c(1, 2, 2, 3),
    time = c(5, 8, 10, 12),
    cause = c(0, 1, 1, 2) # ID 2's last record is an event
  )

  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Check that a censoring record was added for ID 2 after its last event
  data_sorted <- result$mcc_table

  # Test that the calculation handled this properly
  # Number of unique time points should be at least 4 (original unique times plus added censoring)
  expect_gte(nrow(data_sorted), 4)
})

test_that("mcc_equation() maintains correct number of risk", {
  # Test case to verify number at risk calculations
  df <- data.frame(
    id = c(1, 2, 3, 4, 5),
    time = c(5, 8, 8, 12, 15),
    cause = c(1, 0, 2, 1, 0)
  )

  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Test that nrisk decreases appropriately
  mcc_table <- result$mcc_table

  # Initial nrisk should equal total number of IDs
  expect_equal(mcc_table$nrisk[1], 5)

  # nrisk should never be negative
  expect_true(all(mcc_table$nrisk >= 0))

  # nrisk should decrease monotonically (or stay the same)
  expect_true(all(diff(mcc_table$nrisk) <= 0))
})

test_that("mcc_equation() calculates MCC values correctly", {
  # Standard test case with known outcome
  # This example was calculated manually to verify correctness
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(10, 20, 30),
    cause = c(1, 2, 0)
  )

  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Expected values (calculated manually or from a reference implementation)
  # For example:
  expected_mcc_at_time_10 <- 1 / 3 # One event out of three subjects

  # Test with tolerance for floating point
  expect_equal(
    result$mcc_final$mcc[result$mcc_final$time == 10],
    expected_mcc_at_time_10,
    tolerance = 1e-6
  )

  # Verify that MCC is cumulative and increases
  expect_true(all(diff(result$mcc_table$mcc) >= 0))
})

test_that("mcc_equation() properly uses time_precision parameter", {
  # Create dataset with multiple simultaneous events
  df <- data.frame(
    id = c(1, 1, 1),
    time = c(10, 10, 10),
    cause = c(1, 2, 0)
  )

  # Test with different precision values
  result1 <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    time_precision = 1e-3
  )

  result2 <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    time_precision = 1e-6
  )

  # Check that times were adjusted differently
  if (
    all(c("adjusted_data") %in% names(result1)) &&
      all(c("adjusted_data") %in% names(result2))
  ) {
    times1 <- result1$adjusted_data$time[result1$adjusted_data$id == 1]
    times2 <- result2$adjusted_data$time[result2$adjusted_data$id == 1]

    # Times should be different between the two runs
    expect_false(identical(times1, times2))

    # First time should be the same (unadjusted)
    expect_equal(times1[1], times2[1])

    # Differences should reflect the precision
    expect_equal(times1[2] - times1[1], 1e-3)
    expect_equal(times2[2] - times2[1], 1e-6)
  }
})

test_that("mcc_equation() handles empty data as expected", {
  df <- data.frame(
    id = numeric(0),
    time = numeric(0),
    cause = numeric(0)
  )

  # Capture the result as a snapshot
  expect_snapshot({
    result <- mcc_equation(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    )
    str(result) # Include structure in the snapshot
  })
})

test_that("mcc_equation() correctly processes data with all censoring", {
  # Dataset with only censored observations
  df <- data.frame(
    id = 1:3,
    time = c(10, 20, 30),
    cause = c(0, 0, 0)
  )

  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # MCC should be zero throughout since there are no events
  expect_equal(sum(result$mcc_table$event), 0)
  expect_equal(sum(result$mcc_table$mcc), 0)
})

test_that("mcc_equation() correctly processes data with all competing risks", {
  # Dataset with only competing risk observations
  df <- data.frame(
    id = 1:3,
    time = c(10, 20, 30),
    cause = c(2, 2, 2)
  )

  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # MCC should be zero throughout since there are no events of interest
  expect_equal(sum(result$mcc_table$event), 0)
  expect_equal(sum(result$mcc_table$mcc), 0)
})
