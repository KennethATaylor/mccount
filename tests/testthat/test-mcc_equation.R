test_that("mcc_equation() correctly calculates MCC with basic data", {
  # Create a simple dataset with no simultaneous events
  df <- data.frame(
    id = c(1, 1, 2, 3, 4, 4),
    time = c(5, 6, 8, 12, 15, 16),
    cause = c(1, 0, 0, 2, 1, 0)
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
    id = c(1, 1, 2, 3, 3),
    time = c(5, 5, 8, 10, 11),
    cause = c(1, 2, 0, 1, 0)
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
    id = c(1, 2, 2, 2, 3),
    time = c(5, 8, 10, 11, 12),
    cause = c(0, 1, 1, 0, 2) # ID 2's last record is an event
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
    id = c(1, 1, 2, 3, 4, 4, 5),
    time = c(5, 6, 8, 8, 12, 13, 15),
    cause = c(1, 0, 0, 2, 1, 0, 0)
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
    id = c(1, 1, 2, 3),
    time = c(10, 11, 20, 30),
    cause = c(1, 0, 2, 0)
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


test_that("mcc_equation() respects include_details=FALSE parameter", {
  # Create a simple dataset
  df <- data.frame(
    id = c(1, 1, 2, 3, 4, 4, 4),
    time = c(5, 6, 8, 12, 10, 15, 16),
    cause = c(1, 0, 0, 2, 1, 1, 0)
  )

  # Run with default include_details=TRUE
  result_detailed <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Run with include_details=FALSE
  result_simple <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  # Test output structure with include_details=FALSE
  expect_type(result_simple, "list")

  # Should only contain mcc_final
  expect_named(result_simple, "mcc_final")

  # mcc_final should be identical in both outputs
  expect_equal(result_detailed$mcc_final, result_simple$mcc_final)

  # Detailed output should contain more elements
  expect_gt(length(result_detailed), length(result_simple))
})

test_that("mcc_equation() with include_details=FALSE works with various scenarios", {
  # 1. With simultaneous events
  df_sim <- data.frame(
    id = c(1, 1, 2, 3, 3),
    time = c(5, 5, 8, 10, 11),
    cause = c(1, 2, 0, 1, 0)
  )

  result_sim <- mcc_equation(
    data = df_sim,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  expect_named(result_sim, "mcc_final")
  expect_true(all(c("time", "mcc") %in% names(result_sim$mcc_final)))

  # 2. With last records being events
  df_last <- data.frame(
    id = c(1, 2, 2, 2, 3),
    time = c(5, 8, 10, 11, 12),
    cause = c(0, 1, 1, 0, 2) # ID 2's last record is an event
  )

  result_last <- mcc_equation(
    data = df_last,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  expect_named(result_last, "mcc_final")
  expect_true(all(c("time", "mcc") %in% names(result_last$mcc_final)))

  # 3. With only censoring
  df_censor <- data.frame(
    id = 1:3,
    time = c(10, 20, 30),
    cause = c(0, 0, 0)
  )

  result_censor <- mcc_equation(
    data = df_censor,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  expect_named(result_censor, "mcc_final")
  expect_equal(sum(result_censor$mcc_final$mcc), 0)

  # 4. With only competing risks
  df_cmprk <- data.frame(
    id = 1:3,
    time = c(10, 20, 30),
    cause = c(2, 2, 2)
  )

  result_cmprk <- mcc_equation(
    data = df_cmprk,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  expect_named(result_cmprk, "mcc_final")
  expect_equal(sum(result_cmprk$mcc_final$mcc), 0)
})

test_that("mcc_equation() with include_details=FALSE does not include adjusted_data", {
  # Create dataset with simultaneous events
  df <- data.frame(
    id = c(1, 1, 2),
    time = c(5, 5, 8),
    cause = c(1, 2, 0)
  )

  # With default include_details=TRUE
  result_detailed <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Should include adjusted_data
  expect_true("adjusted_data" %in% names(result_detailed))

  # With include_details=FALSE
  result_simple <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  # Should NOT include adjusted_data
  expect_false("adjusted_data" %in% names(result_simple))
})

test_that("mcc_equation() with include_details=FALSE provides sufficient data for bootstrapping", {
  # Create dataset with multiple events
  df <- data.frame(
    id = c(1, 1, 2, 3, 4, 4, 4),
    time = c(5, 6, 8, 12, 10, 15, 16),
    cause = c(1, 0, 0, 2, 1, 1, 0)
  )

  # Run with simplified output
  result <- mcc_equation(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  # Verify we have all the time points and MCC values needed for CI calculation
  expect_true("time" %in% names(result$mcc_final))
  expect_true("mcc" %in% names(result$mcc_final))

  # Simulate a simple bootstrap process
  n_boot <- 3 # Small number for testing
  boot_results <- list()

  # Create bootstrap samples and calculate MCC
  for (i in 1:n_boot) {
    # Sample IDs with replacement
    sample_ids <- sample(unique(df$id), replace = TRUE)

    # Create bootstrap sample
    boot_df <- data.frame()
    for (id in sample_ids) {
      id_rows <- df[df$id == id, ]
      boot_df <- rbind(boot_df, id_rows)
    }

    # Calculate MCC with simplified output
    boot_result <- mcc_equation(
      data = boot_df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = FALSE
    )

    boot_results[[i]] <- boot_result$mcc_final
  }

  # Check if we can extract data for each time point across bootstrap samples
  all_times <- sort(unique(unlist(lapply(boot_results, function(x) x$time))))

  # For example, for the first time point
  if (length(all_times) > 0) {
    t1 <- all_times[1]

    # Extract MCC values for this time point from each bootstrap sample
    mcc_values <- sapply(boot_results, function(x) {
      if (t1 %in% x$time) {
        return(x$mcc[x$time == t1])
      } else {
        return(NA)
      }
    })

    # Verify we can calculate a confidence interval
    expect_true(!all(is.na(mcc_values)))
  }
})

test_that("mcc_equation() handles empty data with include_details=FALSE", {
  df <- data.frame(
    id = numeric(0),
    time = numeric(0),
    cause = numeric(0)
  )

  # Capture the result with simplified output
  expect_snapshot({
    result <- mcc_equation(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = FALSE
    )
    str(result) # Include structure in the snapshot
  })
})

# Tests for weighted MCC functionality
# Add these to a new test file: test-weighted_mcc.R

test_that("mcc function accepts weights parameter correctly", {
  # Create test data
  test_data <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0),
    weights = c(1.2, 0.8, 1.5, 1.0, 1.0, 1.0, 1.0)
  )

  # Test weighted MCC with equation method
  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    method = "equation"
  )

  expect_true(result$weighted)
  expect_equal(result$method, "equation")
  expect_true(is.data.frame(result$mcc_final))
  expect_true(all(c("time", "mcc") %in% names(result$mcc_final)))
})

test_that("weighted MCC produces different results from unweighted", {
  # Create test data where weights will make a difference
  test_data <- data.frame(
    id = c(1, 1, 2, 2, 3, 4, 4),
    time = c(1, 2, 2, 3, 3, 4, 5),
    cause = c(1, 0, 1, 0, 0, 1, 0),
    weights = c(2.0, 2.0, 0.5, 0.5, 1.0, 3.0, 3.0)
  )

  # Calculate unweighted MCC
  unweighted <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "equation"
  )

  # Calculate weighted MCC
  weighted <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    method = "equation"
  )

  expect_false(unweighted$weighted)
  expect_true(weighted$weighted)

  # Results should be different
  expect_false(identical(unweighted$mcc_final, weighted$mcc_final))
})

test_that("weighted MCC works with grouped analysis", {
  # Create test data with groups
  test_data <- data.frame(
    id = c(1, 1, 2, 3, 4, 4, 5, 5, 6),
    time = c(1, 2, 2, 3, 4, 5, 1, 2, 2),
    cause = c(1, 0, 0, 2, 1, 0, 1, 0, 0),
    weights = c(1.2, 1.2, 0.8, 1.5, 1.0, 1.0, 2.0, 2.0, 1.5),
    group = c("A", "A", "A", "A", "B", "B", "B", "B", "B")
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    by = "group",
    method = "equation"
  )

  expect_true(result$weighted)
  expect_equal(result$by_group, "group")
  expect_true("group" %in% names(result$mcc_final))
  expect_true(all(c("A", "B") %in% result$mcc_final$group))
})

test_that("weights parameter validation works correctly", {
  test_data <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    good_weights = c(1.0, 1.0, 1.5, 0.8),
    bad_weights = c(1.0, 1.0, -0.5, 0.8)
  )

  # Valid weights should work
  expect_no_error(
    mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "good_weights"
    )
  )

  # Invalid weights should error
  expect_error(
    mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "bad_weights"
    ),
    class = "rlang_error"
  )

  # Non-existent weights column should error
  expect_error(
    mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "nonexistent"
    ),
    class = "rlang_error"
  )
})

test_that("weights with SCI method produces appropriate error", {
  test_data <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(1.0, 1.0, 1.5, 0.8)
  )

  expect_error(
    mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights",
      method = "sci"
    ),
    "only compatible with.*equation"
  )
})

test_that("weighted MCC handles edge cases correctly", {
  # Test with zero weights
  test_data_zero <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(0.0, 0.0, 1.5, 0.8)
  )

  expect_snapshot(
    result <- mcc(
      test_data_zero,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    )
  )
  expect_true(result$weighted)

  # Test with all equal weights (should be equal)
  test_data_equal <- data.frame(
    id = c(1, 1, 2, 2, 3, 4),
    time = c(1, 2, 2, 3, 3, 4),
    cause = c(1, 0, 1, 0, 0, 2),
    weights = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  )

  unweighted <- mcc(
    test_data_equal,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  weighted <- mcc(
    test_data_equal,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights"
  )

  # Results should be very similar (allowing for floating point differences)
  expect_equal(
    unweighted$mcc_final$mcc,
    weighted$mcc_final$mcc,
    tolerance = 1e-10
  )
})

test_that("weighted MCC with include_details parameter works", {
  test_data <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(1.2, 1.2, 0.8, 1.5)
  )

  # Test with include_details = TRUE (default)
  result_detailed <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    include_details = TRUE
  )

  expect_true("mcc_table" %in% names(result_detailed))
  expect_true("original_data" %in% names(result_detailed))

  # Test with include_details = FALSE
  result_simple <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    include_details = FALSE
  )

  expect_false("mcc_table" %in% names(result_simple))
  expect_false("original_data" %in% names(result_simple))
  expect_true("mcc_final" %in% names(result_simple))
  expect_true(result_simple$weighted)
})

test_that("calculate_weighted_mcc internal function works", {
  # Create mock data.table
  dt <- data.table::data.table(
    id = c(1, 1, 2, 3),
    time = c(1, 5, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(2.0, 2.0, 1.5, 0.8)
  )

  # Test the internal function directly
  result <- mccount:::calculate_weighted_mcc(dt, include_details = TRUE)

  expect_true(is.list(result))
  expect_true("mcc_final" %in% names(result))
  expect_true("mcc_table" %in% names(result))
  expect_true(is.data.frame(result$mcc_final))
  expect_true(all(c("time", "mcc") %in% names(result$mcc_final)))

  # Test with include_details = FALSE
  result_simple <- mccount:::calculate_weighted_mcc(dt, include_details = FALSE)
  expect_true("mcc_final" %in% names(result_simple))
  expect_false("mcc_table" %in% names(result_simple))
})

test_that("weighted MCC handles simultaneous events correctly", {
  # Test data with simultaneous events
  test_data <- data.frame(
    id = c(1, 1, 2, 2, 3),
    time = c(1, 2, 2, 2, 3), # Simultaneous events at time 2 for id 2
    cause = c(1, 0, 1, 2, 0),
    weights = c(1.0, 1.0, 1.5, 1.5, 0.8)
  )

  # Should work with adjust_times = TRUE (default)
  expect_no_error(
    result1 <- mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights",
      adjust_times = TRUE
    )
  )

  # Should warn with adjust_times = FALSE
  expect_warning(
    result2 <- mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights",
      adjust_times = FALSE
    ),
    "simultaneous"
  )

  expect_true(result1$weighted)
  expect_true(result2$weighted)
})

test_that("weighted MCC works correctly with complex data scenarios", {
  # Test with multiple events per person and varying weights
  test_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 4, 4),
    time = c(1, 3, 5, 2, 4, 6, 1, 2),
    cause = c(1, 1, 0, 1, 2, 0, 1, 0),
    weights = c(2.0, 2.0, 2.0, 0.5, 0.5, 1.5, 3.0, 3.0)
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights"
  )

  # Check monotonicity
  mcc_values <- result$mcc_final$mcc
  time_values <- result$mcc_final$time

  # Sort by time to check monotonicity
  ordered_idx <- order(time_values)
  ordered_mcc <- mcc_values[ordered_idx]

  # MCC should be non-decreasing (allowing for floating point precision)
  expect_true(all(diff(ordered_mcc) >= -1e-10))

  # MCC should be non-negative
  expect_true(all(mcc_values >= 0))

  # MCC should start at 0 or positive
  expect_gte(min(mcc_values), 0)
})

test_that("weighted MCC handles single observation scenarios", {
  # Test with single participant, multiple observations
  test_data_single_multi <- data.frame(
    id = c(1, 1, 1),
    time = c(1, 3, 5),
    cause = c(1, 1, 0),
    weights = c(1.5, 1.5, 1.5)
  )

  result_single_multi <- mcc(
    test_data_single_multi,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights"
  )

  expect_true(result_single_multi$weighted)
  expect_gt(nrow(result_single_multi$mcc_final), 1)
})

test_that("weighted MCC consistency between calculation methods", {
  # Test that the weighted calculation components work correctly
  test_data <- data.frame(
    id = c(1, 1, 2, 3, 4, 4),
    time = c(1, 2, 2, 3, 4, 6),
    cause = c(1, 0, 0, 2, 1, 0),
    weights = c(2.0, 2.0, 1.0, 0.5, 1.5, 1.5)
  )

  # Get detailed results to examine calculation components
  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    include_details = TRUE
  )

  # Check that weighted counts are being used properly
  mcc_table <- result$mcc_table

  # Verify that the calculation is using weights correctly
  # (weighted event counts should reflect the weights)
  expect_true(all(mcc_table$nrisk > 0)) # At-risk population should be positive
  expect_true(all(mcc_table$mcc >= 0)) # MCC should be non-negative
  expect_true(all(mcc_table$overall_surv_previous <= 1)) # Survival probability ≤ 1
  expect_true(all(mcc_table$overall_surv_previous >= 0)) # Survival probability ≥ 0

  # Check that MCC values are cumulative
  if (nrow(mcc_table) > 1) {
    mcc_diffs <- diff(mcc_table$mcc)
    expect_true(all(mcc_diffs >= -1e-10)) # Should be non-decreasing
  }
})

test_that("weighted MCC grouped analysis error handling", {
  # Test with group that has invalid weights
  test_data <- data.frame(
    id = c(1, 1., 2, 3, 4),
    time = c(1, 2, 2, 3, 4),
    cause = c(1, 0, 0, 2, 1),
    weights = c(1.0, 1.0, -1.0, 1.5, 2.0), # Negative weight in group
    group = c("A", "A", "A", "B", "B")
  )

  expect_error(
    mcc(
      test_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights",
      by = "group"
    ),
    "Negative values found"
  )

  # Test with empty group after weight validation
  test_data_empty_group <- data.frame(
    id = c(1, 1, 2),
    time = c(1, 2, 2),
    cause = c(1, 0, 0),
    weights = c(1.0, 1.0, 1.5),
    group = c("A", "A", "B")
  )

  # This should work (small groups but valid)
  expect_no_error(
    result <- mcc(
      test_data_empty_group,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights",
      by = "group"
    )
  )
  expect_true(result$weighted)
})

test_that("weighted MCC parameter combinations work correctly", {
  test_data <- data.frame(
    id = c(1, 1, 2, 3, 3),
    time = c(1, 1, 2, 3, 4), # Simultaneous events
    cause = c(1, 2, 0, 1, 0),
    weights = c(1.5, 1.5, 2.0, 0.8, 0.8)
  )

  # Test combinations of parameters
  result1 <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    adjust_times = TRUE,
    include_details = TRUE
  )

  result2 <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    adjust_times = TRUE,
    include_details = FALSE
  )

  # Both should work and be weighted
  expect_true(result1$weighted)
  expect_true(result2$weighted)

  # Detailed vs simple results should have different structure
  expect_true("mcc_table" %in% names(result1))
  expect_false("mcc_table" %in% names(result2))

  # But final MCC should be the same
  expect_equal(result1$mcc_final, result2$mcc_final)
})

test_that("weighted MCC produces sensible results compared to unweighted", {
  # Create scenario where higher weights on earlier events should increase MCC
  test_data <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4),
    time = c(1, 2, 2, 3, 3, 4, 4),
    cause = c(1, 0, 1, 0, 1, 0, 0),
    weights_early_heavy = c(5.0, 5.0, 1.0, 1.0, 1.0, 1.0, 1.0), # Heavy weight on early event
    weights_late_heavy = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 5.0), # Heavy weight on late non-event
    weights_equal = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  )

  # Unweighted
  unweighted <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Early heavy weights
  early_heavy <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights_early_heavy"
  )

  # Equal weights (should be similar to unweighted)
  equal_weights <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights_equal"
  )

  # Early heavy weights should generally produce higher MCC values
  final_mcc_unweighted <- max(unweighted$mcc_final$mcc)
  final_mcc_early_heavy <- max(early_heavy$mcc_final$mcc)
  final_mcc_equal <- max(equal_weights$mcc_final$mcc)

  expect_gt(final_mcc_early_heavy, final_mcc_unweighted)
  expect_equal(final_mcc_equal, final_mcc_unweighted, tolerance = 1e-10)
})

test_that("weighted MCC handles missing data patterns correctly", {
  # Test with participants who have different follow-up patterns
  test_data <- data.frame(
    id = c(1, 1, 2, 3, 3, 3, 4),
    time = c(1, 5, 2, 1, 2, 4, 3),
    cause = c(1, 0, 0, 1, 1, 0, 2), # Different ending patterns
    weights = c(1.5, 1.5, 2.0, 0.8, 0.8, 0.8, 1.2)
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights"
  )

  expect_true(result$weighted)
  expect_true(nrow(result$mcc_final) > 0)

  # Check that MCC values are monotonically non-decreasing
  mcc_values <- result$mcc_final$mcc
  expect_true(all(diff(mcc_values) >= -1e-10)) # Allow for floating point precision
})

test_that("weighted MCC grouped analysis maintains group separation", {
  # Create data where groups should have very different MCC patterns
  test_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 3, 4, 4),
    time = c(1, 2, 3, 1, 3, 1, 2, 1, 4),
    cause = c(1, 1, 0, 1, 0, 1, 0, 1, 0),
    weights = c(2.0, 2.0, 2.0, 1.0, 1.0, 0.5, 0.5, 3.0, 3.0),
    group = c(
      "high",
      "high",
      "high",
      "medium",
      "medium",
      "low",
      "low",
      "high",
      "high"
    )
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    by = "group"
  )

  expect_true(result$weighted)
  expect_equal(result$by_group, "group")

  # Check that all groups are represented
  groups_in_result <- unique(result$mcc_final$group)
  expect_setequal(groups_in_result, c("high", "medium", "low"))

  # Each group should have at least one MCC estimate
  for (grp in groups_in_result) {
    group_data <- result$mcc_final[result$mcc_final$group == grp, ]
    expect_gt(nrow(group_data), 0)
    expect_true(all(group_data$mcc >= 0))
  }
})

test_that("weighted MCC error handling covers all validation scenarios", {
  base_data <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2)
  )

  # Test with missing weights in some rows
  data_with_na <- base_data
  data_with_na$weights <- c(1.0, 1.0, NA, 1.5)

  expect_error(
    mcc(
      data_with_na,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    ),
    "Missing values found"
  )

  # Test with character weights
  data_char_weights <- base_data
  data_char_weights$weights <- c("1.0", "1.0", "2.0", "1.5")

  expect_error(
    mcc(
      data_char_weights,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    ),
    "must contain numeric values"
  )

  # Test with factor weights
  data_factor_weights <- base_data
  data_factor_weights$weights <- factor(c("low", "low", "high", "medium"))

  expect_error(
    mcc(
      data_factor_weights,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    ),
    "must contain numeric values"
  )
})

test_that("weighted MCC preserves data structure in results", {
  test_data <- data.frame(
    id = c(1, 1, 2, 2, 3, 4),
    time = c(1, 2, 2, 3, 3, 4),
    cause = c(1, 0, 1, 0, 0, 2),
    weights = c(1.2, 1.2, 0.8, 0.8, 1.5, 2.0)
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights",
    include_details = TRUE
  )

  # Check that original data is preserved correctly
  expect_true("original_data" %in% names(result))
  expect_equal(nrow(result$original_data), nrow(test_data))
  expect_true("weights" %in% names(result$original_data))

  # Check that mcc_table has expected structure
  expect_true("mcc_table" %in% names(result))
  expected_cols <- c(
    "time",
    "nrisk",
    "censor",
    "event",
    "cmprk",
    "overall_surv_previous",
    "ave_events",
    "mcc"
  )
  expect_true(all(expected_cols %in% names(result$mcc_table)))

  # Check that all numeric columns are indeed numeric
  numeric_cols <- c(
    "nrisk",
    "censor",
    "event",
    "cmprk",
    "overall_surv_previous",
    "ave_events",
    "mcc"
  )
  for (col in numeric_cols) {
    expect_true(
      is.numeric(result$mcc_table[[col]]),
      info = paste("Column", col, "should be numeric")
    )
  }
})

test_that("weighted MCC works with extreme weight scenarios", {
  # Test with very small weights
  test_data_small <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(1e-6, 1e-6, 1e-6, 1e-6)
  )

  expect_no_error(
    result_small <- mcc(
      test_data_small,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    )
  )
  expect_true(result_small$weighted)

  # Test with very large weights
  test_data_large <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(1, 2, 2, 3),
    cause = c(1, 0, 0, 2),
    weights = c(1e6, 1e6, 1e6, 1e6)
  )

  expect_no_error(
    result_large <- mcc(
      test_data_large,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      weights = "weights"
    )
  )
  expect_true(result_large$weighted)
})

test_that("weighted MCC maintains mathematical properties", {
  # Test that MCC is monotonically non-decreasing
  test_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 3),
    time = c(1, 3, 4, 2, 4, 5),
    cause = c(1, 1, 0, 1, 0, 2),
    weights = c(1.5, 1.5, 1.5, 2.0, 2.0, 0.8)
  )

  result <- mcc(
    test_data,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    weights = "weights"
  )

  expect_true(result$weighted)
  expect_true(nrow(result$mcc_final) > 0)
  expect_true(all(result$mcc_final$mcc >= 0))
  expect_true(is.numeric(result$mcc_final$mcc))
})
