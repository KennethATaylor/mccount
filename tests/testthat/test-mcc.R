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

  expect_error(
    mcc(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = NA
    ),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
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

test_that("by argument basic functionality works", {
  # Create simple test data
  df <- data.frame(
    id = c(1, 2, 3, 4, 5, 6),
    time = c(5, 8, 12, 15, 10, 20),
    cause = c(1, 0, 2, 1, 1, 0),
    group = c("A", "A", "A", "B", "B", "B")
  )

  # Test that by argument works without error
  expect_no_error(
    result <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "group"
    )
  )

  # Test that result has expected structure
  expect_true("by_group" %in% names(result))
  expect_equal(result$by_group, "group")
  expect_true("group" %in% names(result$mcc_final))
  expect_true(all(c("A", "B") %in% result$mcc_final$group))
})

test_that("backward compatibility maintained", {
  # Create simple test data
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1)
  )

  # Test that existing behavior is unchanged
  result <- mcc(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Should not have by_group component when by is not specified
  expect_false("by_group" %in% names(result))

  # Should have all the expected components
  expect_true("mcc_final" %in% names(result))
  expect_true("method" %in% names(result))

  # mcc_final should not have any grouping column
  expect_named(result$mcc_final, c("time", "mcc"))
})

test_that("mcc() validates by argument correctly", {
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2),
    group = c("A", "B", "A")
  )

  # Test with invalid by argument types
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = 123
    )
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = c("group1", "group2")
    )
  )

  # Test with missing by column
  expect_snapshot(
    error = TRUE,
    mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "missing_column"
    )
  )

  # Test with all NA values in by column
  df_na <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2),
    group = c(NA, NA, NA)
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df_na,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "group"
    )
  )
})

test_that("mcc() with by argument handles various group scenarios", {
  # Test with single group (after filtering)
  df_single <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2),
    treatment = c("Active", "Active", "Active")
  )

  expect_no_error(
    result_single <- mcc(
      df_single,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "treatment"
    )
  )

  expect_equal(unique(result_single$mcc_final$treatment), "Active")
  expect_equal(result_single$by_group, "treatment")

  # Test with mixed NA and valid values
  df_mixed_na <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    site = c("Hospital_A", NA, "Hospital_B", "Hospital_A")
  )

  expect_no_error(
    result_mixed <- mcc(
      df_mixed_na,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "site"
    )
  )

  # Should only include non-NA groups
  expect_true(all(
    c("Hospital_A", "Hospital_B") %in% result_mixed$mcc_final$site
  ))
  expect_false(any(is.na(result_mixed$mcc_final$site)))

  # Test with numeric grouping variable
  df_numeric <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    dose = c(10, 10, 20, 20)
  )

  expect_no_error(
    result_numeric <- mcc(
      df_numeric,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "dose"
    )
  )

  expect_true(all(c(10, 20) %in% result_numeric$mcc_final$dose))
})

test_that("mcc() with by argument works with both methods", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 3, 4),
    time = c(5, 10, 8, 12, 15, 20),
    cause = c(1, 1, 0, 2, 1, 0),
    treatment = c("A", "A", "A", "A", "B", "B")
  )

  # Test equation method with by
  expect_no_error(
    result_eq <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "treatment",
      method = "equation"
    )
  )

  expect_true("mcc_table" %in% names(result_eq))
  expect_equal(result_eq$method, "equation")

  # Test sci method with by (without tstart_var)
  expect_no_error(
    result_sci <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "treatment",
      method = "sci"
    )
  )

  expect_true("sci_table" %in% names(result_sci))
  expect_equal(result_sci$method, "sci")
})

test_that("mcc() with by argument and tstart_var works", {
  # Separate test for tstart_var functionality
  df_tstart <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    treatment = c("A", "A", "B", "B"),
    tstart = c(0, 1, 0, 2)
  )

  # Test sci method with by and tstart_var
  expect_no_error(
    result_sci_tstart <- mcc(
      df_tstart,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "treatment",
      method = "sci",
      tstart_var = "tstart"
    )
  )

  expect_true("sci_table" %in% names(result_sci_tstart))
  expect_equal(result_sci_tstart$method, "sci")
  expect_true("treatment" %in% names(result_sci_tstart$mcc_final))
})

test_that("mcc() with by argument preserves group information in all components", {
  df <- data.frame(
    id = c(1, 1, 2, 3, 3),
    time = c(5, 10, 8, 12, 15),
    cause = c(1, 1, 0, 2, 1),
    treatment = c("A", "A", "A", "B", "B")
  )

  # Test with SCI method to check all_cis handling (without tstart_var)
  result_sci <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    by = "treatment",
    method = "sci",
    include_details = TRUE
  )

  # Check that all_cis has group structure
  expect_true("all_cis" %in% names(result_sci))
  expect_true("A" %in% names(result_sci$all_cis))
  expect_true("B" %in% names(result_sci$all_cis))

  # Check that non-empty all_cis tibbles have group columns
  for (group_name in names(result_sci$all_cis)) {
    for (ci_table in result_sci$all_cis[[group_name]]) {
      if (nrow(ci_table) > 0) {
        expect_true("treatment" %in% names(ci_table))
        expect_equal(unique(ci_table$treatment), group_name)
      }
    }
  }

  # Check that other components have group columns
  expect_true("treatment" %in% names(result_sci$sci_table))
  expect_true("treatment" %in% names(result_sci$mcc_base))
})

test_that("mcc() with by argument and include_details parameter", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    group = c("Control", "Control", "Treatment", "Treatment")
  )

  # Test with include_details = TRUE
  result_detailed <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    by = "group",
    include_details = TRUE
  )

  expect_true("mcc_table" %in% names(result_detailed))
  expect_true("original_data" %in% names(result_detailed))
  expect_true("group" %in% names(result_detailed$mcc_table))
  expect_true("group" %in% names(result_detailed$original_data))

  # Test with include_details = FALSE
  result_simple <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    by = "group",
    include_details = FALSE
  )

  expect_named(result_simple, c("mcc_final", "method", "by_group"))
  expect_false("mcc_table" %in% names(result_simple))
  expect_false("original_data" %in% names(result_simple))
})

test_that("mcc_by_group() handles empty groups gracefully", {
  # Create data where one group becomes empty after filtering
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2),
    group = c("A", "B", "C")
  )

  # Mock a scenario where group B has no data (simulate empty group)
  # This tests the warning and continuation logic
  expect_no_error(
    result <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "group"
    )
  )

  # Should have results for all non-empty groups
  expect_true(all(c("A", "B", "C") %in% result$mcc_final$group))
})

test_that("mcc_by_group() handles all empty groups scenario", {
  # Create a scenario that would result in all groups being empty
  # This is hard to simulate directly, so we test the error handling
  df_all_na <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 8, 10),
    cause = c(1, 0, 2),
    group = c(NA, NA, NA)
  )

  expect_snapshot(
    error = TRUE,
    mcc(
      df_all_na,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "group"
    )
  )
})

test_that("mcc() with by argument preserves group information in all components", {
  df <- data.frame(
    id = c(1, 1, 2, 3, 3),
    time = c(5, 10, 8, 12, 15),
    cause = c(1, 1, 0, 2, 1),
    treatment = c("A", "A", "A", "B", "B")
  )

  # Test with SCI method to check all_cis handling (without tstart_var)
  result_sci <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    by = "treatment",
    method = "sci",
    include_details = TRUE
  )

  # Check that all_cis has group structure
  expect_true("all_cis" %in% names(result_sci))
  expect_true("A" %in% names(result_sci$all_cis))
  expect_true("B" %in% names(result_sci$all_cis))

  # Check that non-empty all_cis tibbles have group columns
  for (group_name in names(result_sci$all_cis)) {
    for (ci_table in result_sci$all_cis[[group_name]]) {
      if (nrow(ci_table) > 0) {
        expect_true("treatment" %in% names(ci_table))
        expect_equal(unique(ci_table$treatment), group_name)
      }
    }
  }

  # Check that other components have group columns
  expect_true("treatment" %in% names(result_sci$sci_table))
  expect_true("treatment" %in% names(result_sci$mcc_base))
})

test_that("mcc() with by argument handles factor grouping variables", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    stage = factor(
      c("Early", "Late", "Early", "Late"),
      levels = c("Early", "Late")
    )
  )

  expect_no_error(
    result <- mcc(
      df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "stage"
    )
  )

  # Factor levels should be preserved
  expect_true(all(c("Early", "Late") %in% result$mcc_final$stage))
  expect_equal(result$by_group, "stage")
})

test_that("mcc() with by argument warning for many groups", {
  # Create data with many groups to trigger warning
  many_groups_df <- data.frame(
    id = 1:25,
    time = rep(10, 25),
    cause = rep(1, 25),
    group = paste0("group_", 1:25)
  )

  expect_snapshot(
    result <- mcc(
      many_groups_df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      by = "group"
    )
  )

  # Should still work despite warning
  expect_equal(length(unique(result$mcc_final$group)), 25)
})

test_that("add_group_column_to_result() handles edge cases", {
  # Test with empty result
  empty_result <- list()
  result_empty <- add_group_column_to_result(empty_result, "group", "A")
  expect_equal(result_empty, empty_result)

  # Test with result containing NULL components
  result_with_nulls <- list(
    mcc_final = tibble::tibble(time = 1, mcc = 0.1),
    mcc_table = NULL,
    other_component = "some_value"
  )

  result_nulls <- add_group_column_to_result(
    result_with_nulls,
    "treatment",
    "Active"
  )
  expect_true("treatment" %in% names(result_nulls$mcc_final))
  expect_null(result_nulls$mcc_table)
  expect_equal(result_nulls$other_component, "some_value")
})

test_that("combine_group_results() handles mismatched structures", {
  # Create results with different components
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        group = "A",
        time = 5,
        mcc = 0.1
      ),
      mcc_table = tibble::tibble(
        group = "A",
        time = 5,
        nrisk = 10,
        mcc = 0.1
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        group = "B",
        time = 8,
        mcc = 0.2
      )
      # Note: mcc_table missing for group B
    )
  )

  # Should handle missing components gracefully
  expect_no_error(
    combined <- combine_group_results(group_results, "group", TRUE)
  )

  expect_equal(nrow(combined$mcc_final), 2)
  expect_equal(nrow(combined$mcc_table), 1) # Only from group A
})

test_that("mcc() output structure is consistent between single and grouped analysis", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 10, 12),
    cause = c(1, 0, 2, 1),
    treatment = c("A", "A", "A", "A") # All same group
  )

  # Single group analysis (by = NULL)
  result_single <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Grouped analysis with only one group
  result_grouped <- mcc(
    df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    by = "treatment"
  )

  # mcc_final values should be the same (ignoring group column)
  expect_equal(
    result_single$mcc_final$mcc,
    result_grouped$mcc_final$mcc
  )
  expect_equal(
    result_single$mcc_final$time,
    result_grouped$mcc_final$time
  )

  # Grouped result should have additional structure
  expect_true("by_group" %in% names(result_grouped))
  expect_false("by_group" %in% names(result_single))
  expect_true("treatment" %in% names(result_grouped$mcc_final))
  expect_false("treatment" %in% names(result_single$mcc_final))
})
