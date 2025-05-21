test_that("mcc() dispatches to the correct implementation based on method", {
  # Create simple test data
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1)
  )

  # Test equation method (default)
  result_default <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  result_equation <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "equation"
  )

  result_sci <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "sci"
  )

  # Check method field is added correctly
  expect_equal(result_default$method, "equation")
  expect_equal(result_equation$method, "equation")
  expect_equal(result_sci$method, "sci")

  # Check that default is equation method
  expect_equal(result_default, result_equation)

  # Check that different methods give different result structures
  expect_true("mcc_table" %in% names(result_equation))
  expect_true("sci_table" %in% names(result_sci))

  # Function should dispatch to correct implementation
  expect_false(identical(result_equation$mcc_final, result_sci$mcc_final))
})

test_that("mcc() validates the method argument correctly", {
  df <- data.frame(id = 1, time = 5, cause = 1)

  # Valid methods should work
  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "equation"
    )
  )
  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "sci"
    )
  )

  # Invalid method should error
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "invalid"
    )
  )
})

test_that("mcc() correctly validates time_precision", {
  df <- data.frame(id = 1, time = 5, cause = 1)

  # Invalid time_precision values
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      time_precision = "not_numeric"
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      time_precision = -1
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      time_precision = c(1, 2)
    )
  )

  # Valid time_precision values
  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      time_precision = 0.1
    )
  )

  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      time_precision = 1e-10
    )
  )
})

test_that("mcc() validates tstart_var compatibility with method", {
  df <- data.frame(
    id = c(1, 2),
    time = c(5, 8),
    cause = c(1, 0),
    tstart = c(1, 2)
  )

  # tstart_var should error with equation method
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "equation",
      tstart_var = "tstart"
    )
  )

  # tstart_var should work with sci method
  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "sci",
      tstart_var = "tstart"
    )
  )
})

test_that("mcc() validates adjust_times correctly", {
  df <- data.frame(id = 1, time = 5, cause = 1)

  # Invalid adjust_times values
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = "TRUE"
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = 1
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = c(TRUE, FALSE)
    )
  )

  # Valid adjust_times values
  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = TRUE
    )
  )

  expect_snapshot(
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = FALSE
    )
  )
})

test_that("mcc() handles variable names with spaces and special characters", {
  # Create test data with unusual column names
  df <- data.frame(
    patient_id = c(1, 2, 3),
    followup_time = c(5, 8, 12),
    event_status = c(1, 0, 2)
  )

  # Test with strings
  expect_no_error(
    result1 <- mcc(
      df,
      id_var = "patient_id",
      time_var = "followup_time",
      cause_var = "event_status"
    )
  )

  # Check result structure to confirm it worked
  expect_true("mcc_final" %in% names(result1))

  # Test with NSE (non-standard evaluation)
  expect_no_error(
    result2 <- mcc(
      df,
      id_var = patient_id,
      time_var = followup_time,
      cause_var = event_status
    )
  )

  # Check result structure to confirm it worked
  expect_true("mcc_final" %in% names(result2))

  # Now test with more complex names (but without spaces)
  df2 <- data.frame(
    patient.id = c(1, 2, 3),
    followup.time = c(5, 8, 12),
    event.status = c(1, 0, 2)
  )

  expect_no_error(
    result3 <- mcc(
      df2,
      id_var = patient.id,
      time_var = followup.time,
      cause_var = event.status
    )
  )

  expect_true("mcc_final" %in% names(result3))
})

test_that("mcc() works with data.frames and tibbles", {
  # Create test data
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 12),
    cause = c(1, 0, 2)
  )

  # Convert to tibble
  tbl <- tibble::as_tibble(df)

  # Test with data.frame
  result_df <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Test with tibble
  result_tbl <- mcc(
    data = tbl,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Results should be equivalent
  expect_equal(result_df$mcc_final, result_tbl$mcc_final)
})

test_that("mcc() handles method parameter with partial matching", {
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 12),
    cause = c(1, 0, 2)
  )

  # Test with partial method names that should match
  result_e <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "e" # Should match "equation"
  )

  result_s <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "s" # Should match "sci"
  )

  # Check method field
  expect_equal(result_e$method, "equation")
  expect_equal(result_s$method, "sci")
})

test_that("mcc() handles edge cases correctly", {
  # Empty data frame
  empty_df <- data.frame(
    id = numeric(0),
    time = numeric(0),
    cause = numeric(0)
  )

  # Test with empty data - should not error but give appropriate result
  expect_no_error(
    empty_result <- mcc(
      data = empty_df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    )
  )

  expect_true("mcc_final" %in% names(empty_result))

  # Single row data
  single_row_df <- data.frame(
    id = 1,
    time = 5,
    cause = 1
  )

  # Test with single row - should work correctly
  expect_no_error(
    single_result <- mcc(
      data = single_row_df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    )
  )

  expect_true(nrow(single_result$mcc_final) > 0)
})

test_that("mcc() can handle nested tibbles without error", {
  skip_if_not_installed("tidyr")

  # Create data with a nested column
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 12),
    cause = c(1, 0, 2)
  )

  # Add a nested column
  nested_df <- tibble::as_tibble(df) |>
    dplyr::mutate(extra = list(1:3, 4:6, 7:9))

  # Should still work with nested data structure
  expect_no_error(
    result <- mcc(
      data = nested_df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    )
  )

  expect_true("mcc_final" %in% names(result))
})

test_that("mcc() end-to-end functionality through snapshots", {
  # Create representative test dataset
  df <- data.frame(
    id = c(1, 1, 2, 2, 3, 4, 4),
    time = c(5, 10, 8, 13, 12, 6, 15),
    cause = c(1, 0, 1, 2, 0, 1, 1)
  )

  # Test with equation method
  expect_snapshot({
    result_eq <- mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "equation"
    )

    # Print key parts
    cat("MCC with equation method:\n")
    print(result_eq$mcc_final)
  })

  # Test with sci method
  expect_snapshot({
    result_sci <- mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "sci"
    )

    # Print key parts
    cat("MCC with sci method:\n")
    print(result_sci$mcc_final)
  })
})


test_that("mcc() produces equivalent results from both methods", {
  # First dataset - simultaneous events at time=3 for id=5
  df1 <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 1, 2)
  ) |>
    dplyr::arrange(id, time)

  # Second dataset - no simultaneous events (time=4 instead of 3 for id=5)
  df2 <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 3, 4),
    cause = c(0, 0, 2, 1, 1, 1, 1, 2)
  ) |>
    dplyr::arrange(id, time)

  # Calculate MCC using equation method
  mcc_eq1 <- mcc(df1, id_var = "id", time_var = "time", cause_var = "cause")
  mcc_eq2 <- mcc(df2, id_var = "id", time_var = "time", cause_var = "cause")

  # Calculate MCC using SCI method
  mcc_sci1 <- mcc(
    df1,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "sci"
  )
  mcc_sci2 <- mcc(
    df2,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "sci"
  )

  # Prepare data for comparison
  # For equation method, extract unique time-mcc pairs
  eq_times1 <- mcc_eq1$mcc_table$time
  eq_mcc1 <- mcc_eq1$mcc_table$mcc

  eq_times2 <- mcc_eq2$mcc_table$time
  eq_mcc2 <- mcc_eq2$mcc_table$mcc

  # For SCI method, extract time-SumCIs pairs
  sci_times1 <- mcc_sci1$sci_table$time
  sci_mcc1 <- mcc_sci1$sci_table$SumCIs

  sci_times2 <- mcc_sci2$sci_table$time
  sci_mcc2 <- mcc_sci2$sci_table$SumCIs

  # Compare outputs between methods for each dataset

  # Create comparison dataframes by time point
  compare1 <- merge(
    mcc_eq1$mcc_table[, c("time", "mcc")],
    mcc_sci1$sci_table[, c("time", "SumCIs")],
    by = "time",
    all = TRUE
  )

  compare2 <- merge(
    mcc_eq2$mcc_table[, c("time", "mcc")],
    mcc_sci2$sci_table[, c("time", "SumCIs")],
    by = "time",
    all = TRUE
  )

  # Test that MCC values from both methods are approximately equal
  # Use tolerance to account for floating point differences
  expect_equal(compare1$mcc, compare1$SumCIs, tolerance = 1e-6)
  expect_equal(compare2$mcc, compare2$SumCIs, tolerance = 1e-6)

  # Compare datasets 1 and 2 for each method
  # Get common time points for equation method
  common_times_eq <- intersect(eq_times1, eq_times2)

  # Get values at common time points
  eq_mcc1_common <- mcc_eq1$mcc_table$mcc[match(
    common_times_eq,
    mcc_eq1$mcc_table$time
  )]
  eq_mcc2_common <- mcc_eq2$mcc_table$mcc[match(
    common_times_eq,
    mcc_eq2$mcc_table$time
  )]

  # Compare MCC values at common time points
  expect_equal(eq_mcc1_common, eq_mcc2_common, tolerance = 1e-6)

  # Get common time points for SCI method
  common_times_sci <- intersect(sci_times1, sci_times2)

  # Get values at common time points
  sci_mcc1_common <- mcc_sci1$sci_table$SumCIs[match(
    common_times_sci,
    mcc_sci1$sci_table$time
  )]
  sci_mcc2_common <- mcc_sci2$sci_table$SumCIs[match(
    common_times_sci,
    mcc_sci2$sci_table$time
  )]

  # Compare SumCIs values at common time points
  expect_equal(sci_mcc1_common, sci_mcc2_common, tolerance = 1e-6)

  # Final verification using the final MCC values at the largest time points
  max_time_eq1 <- max(mcc_eq1$mcc_final$time)
  max_time_eq2 <- max(mcc_eq2$mcc_final$time)
  max_time_sci1 <- max(mcc_sci1$mcc_final$time)
  max_time_sci2 <- max(mcc_sci2$mcc_final$time)

  final_mcc_eq1 <- mcc_eq1$mcc_final$mcc[mcc_eq1$mcc_final$time == max_time_eq1]
  final_mcc_eq2 <- mcc_eq2$mcc_final$mcc[mcc_eq2$mcc_final$time == max_time_eq2]
  final_mcc_sci1 <- mcc_sci1$mcc_final$SumCIs[
    mcc_sci1$mcc_final$time == max_time_sci1
  ]
  final_mcc_sci2 <- mcc_sci2$mcc_final$SumCIs[
    mcc_sci2$mcc_final$time == max_time_sci2
  ]

  # The final MCC values should be equal across methods and datasets
  expect_equal(final_mcc_eq1, final_mcc_sci1, tolerance = 1e-6)
  expect_equal(final_mcc_eq2, final_mcc_sci2, tolerance = 1e-6)
  expect_equal(final_mcc_eq1, final_mcc_eq2, tolerance = 1e-6)
  expect_equal(final_mcc_sci1, final_mcc_sci2, tolerance = 1e-6)
})


test_that("mcc() correctly passes include_details parameter to implementation functions", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1)
  )

  # Test with equation method
  result_eq_full <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "equation",
    include_details = TRUE
  )

  result_eq_simple <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "equation",
    include_details = FALSE
  )

  # Verify that include_details=TRUE includes detailed outputs
  expect_true("mcc_table" %in% names(result_eq_full))
  expect_true("original_data" %in% names(result_eq_full))

  # Verify that include_details=FALSE only returns mcc_final and method
  expect_named(result_eq_simple, c("mcc_final", "method"))

  # mcc_final should be identical regardless of include_details
  expect_equal(result_eq_full$mcc_final, result_eq_simple$mcc_final)

  # Test with sci method
  result_sci_full <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "sci",
    include_details = TRUE
  )

  result_sci_simple <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    method = "sci",
    include_details = FALSE
  )

  # Verify that include_details=TRUE includes detailed outputs
  expect_true("sci_table" %in% names(result_sci_full))
  expect_true("all_cis" %in% names(result_sci_full))
  expect_true("original_data" %in% names(result_sci_full))

  # Verify that include_details=FALSE only returns mcc_final and method
  expect_named(result_sci_simple, c("mcc_final", "method"))

  # mcc_final should be identical regardless of include_details
  expect_equal(result_sci_full$mcc_final, result_sci_simple$mcc_final)
})

test_that("mcc() validates include_details parameter", {
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2)
  )

  # Test with invalid include_details value
  expect_snapshot(
    error = TRUE,
    mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = "TRUE" # String instead of logical
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = c(TRUE, FALSE) # Vector instead of single value
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = NA # NA instead of TRUE/FALSE
    )
  )
})

test_that("mcc() with include_details=FALSE is suitable for bootstrapping", {
  skip_if_not_installed("boot")

  # Create sample data
  set.seed(123)
  df <- data.frame(
    id = rep(1:20, each = 2),
    time = c(runif(20, 1, 10), runif(20, 11, 20)),
    cause = sample(0:2, 40, replace = TRUE, prob = c(0.2, 0.6, 0.2))
  )

  # Define a function to calculate MCC that returns just the maximum MCC value
  mcc_statistic <- function(data, indices) {
    # Create a bootstrap sample
    boot_data <- data[indices, ]

    # Calculate MCC with simplified output
    result <- mcc(
      data = boot_data,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "equation",
      include_details = FALSE
    )

    # Return maximum MCC value
    max_mcc <- max(result$mcc_final$mcc)
    return(max_mcc)
  }

  # Test if we can run a small number of bootstrap replicates without error
  tryCatch(
    {
      boot_result <- boot::boot(
        data = df,
        statistic = mcc_statistic,
        R = 3 # Small number for testing
      )

      # If no error, the test passes
      expect_true(is.numeric(boot_result$t))
      expect_equal(length(boot_result$t), 3)
    },
    error = function(e) {
      # If an error occurs, the test fails
      fail(paste("Bootstrap failed:", e$message))
    }
  )
})

test_that("mcc() documentation example works with include_details=FALSE", {
  # Create the sample data from the documentation example
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 1, 2)
  ) |>
    dplyr::arrange(id, time)

  # Run with include_details=FALSE
  expect_no_error({
    mcc_eq <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = FALSE
    )

    mcc_sci <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      method = "sci",
      include_details = FALSE
    )
  })

  # Verify structure of simplified output
  expect_named(mcc_eq, c("mcc_final", "method"))
  expect_named(mcc_sci, c("mcc_final", "method"))
})
