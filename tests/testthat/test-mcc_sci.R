test_that("mcc_sci() correctly calculates MCC with basic data", {
  # Create a simple dataset with no simultaneous events
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1)
  )

  # Run the function
  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Test structure of output
  expect_type(result, "list")
  expect_named(
    result,
    c("mcc_final", "sci_table", "all_cis", "mcc_base", "original_data"),
    ignore.order = TRUE
  )

  # Test mcc_final structure
  expect_s3_class(result$mcc_final, c("tbl_df", "tbl", "data.frame"))
  expect_named(result$mcc_final, c("time", "SumCIs"))

  # Test sci_table structure
  expect_s3_class(result$sci_table, c("tbl_df", "tbl", "data.frame"))
  expect_true("SumCIs" %in% names(result$sci_table))
  expect_true("time" %in% names(result$sci_table))

  # Test calculation correctness
  expect_true(all(result$mcc_final$SumCIs >= 0))

  # Test that MCC is correctly sorted by time
  expect_true(all(diff(result$mcc_final$time) >= 0))
})

test_that("mcc_sci() handles tstart_var correctly", {
  # Create dataset with tstart values
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(5, 8, 12, 15),
    cause = c(1, 0, 2, 1),
    tstart = c(1, 2, 3, 5)
  )

  # Run with tstart_var
  result_with_tstart <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    tstart_var = "tstart"
  )

  # Run without tstart_var for comparison
  result_no_tstart <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Check that tstart is correctly used
  expect_true("tstart" %in% names(result_with_tstart$original_data))
  expect_false(all(result_with_tstart$original_data$tstart == 0))

  # Results should differ when using tstart vs not using it
  expect_false(identical(
    result_with_tstart$mcc_final$SumCIs,
    result_no_tstart$mcc_final$SumCIs
  ))
})

test_that("mcc_sci() correctly handles left-truncated data", {
  skip_if_not_installed("mstate")
  skip_if_not_installed("survival")

  # Dataset with left truncation
  df <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(10, 15, 20, 25),
    cause = c(1, 2, 1, 0),
    tstart = c(5, 8, 10, 12) # All observations start after time 0
  )

  # Calculate MCC
  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    tstart_var = "tstart"
  )

  # Check that calc_trunc is detected
  # We can infer this by examining the structure of the result
  expect_true("mcc_final" %in% names(result))
  expect_true("SumCIs" %in% names(result$mcc_final))

  # Times in result should be >= minimum start time
  expect_true(all(result$mcc_final$time >= min(df$tstart)))
})

test_that("mcc_sci() handles data with no events of interest", {
  # Dataset with only censoring and competing risks
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 10, 15),
    cause = c(0, 2, 0) # No cause=1 events
  )

  # Should warn about no events and return zero MCC
  expect_warning(
    result <- mcc_sci(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    ),
    "Setting sum of cumulative incidence"
  )

  # Check result structure
  expect_equal(result$mcc_final$SumCIs, 0)
  expect_equal(nrow(result$sci_table), 1)
  expect_equal(result$sci_table$SumCIs, 0)
})

test_that("mcc_sci() correctly handles simultaneous events", {
  # Dataset with simultaneous events
  df <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(5, 5, 8, 10),
    cause = c(1, 2, 0, 1)
  )

  # With adjustment
  result_adj <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    adjust_times = TRUE
  )

  # Without adjustment - expect warning
  expect_warning(
    result_no_adj <- mcc_sci(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      adjust_times = FALSE
    ),
    "events will be processed without time adjustment"
  )

  # Check adjustment was applied
  expect_true("adjusted_data" %in% names(result_adj))
  expect_false("adjusted_data" %in% names(result_no_adj))
})

test_that("mcc_sci() generates correct CI columns", {
  # Create dataset with multiple events per ID
  df <- data.frame(
    id = c(1, 1, 2, 2, 3),
    time = c(5, 10, 8, 15, 12),
    cause = c(1, 1, 1, 0, 2)
  )

  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Check that CI columns are created
  ci_columns <- grep("^CI", names(result$sci_table), value = TRUE)
  expect_gte(length(ci_columns), 1)

  # Check SumCIs is the sum of CI columns
  for (i in 1:nrow(result$sci_table)) {
    ci_sum <- sum(as.numeric(result$sci_table[i, ci_columns]))
    expect_equal(result$sci_table$SumCIs[i], ci_sum)
  }
})

test_that("mcc_sci() processes multiple recurrent events correctly", {
  # Create dataset with multiple events per ID
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 3),
    time = c(5, 10, 15, 8, 12, 7),
    cause = c(1, 1, 1, 1, 0, 2)
  )

  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Check max_events calculation
  all_cis_length <- length(result$all_cis)
  expect_gte(all_cis_length, 2) # Should have at least 2 event levels

  # Check structure of all_cis list
  expect_type(result$all_cis, "list")
  expect_length(result$all_cis, all_cis_length)

  # Check CI columns in sci_table
  expect_gte(sum(grepl("^CI", names(result$sci_table))), all_cis_length)
})

test_that("mcc_sci() handles recurrent events with competing risks", {
  # Create complex dataset with recurrent events and competing risks
  df <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4),
    time = c(5, 10, 8, 12, 6, 15, 9),
    cause = c(1, 2, 1, 1, 0, 1, 2)
  )

  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Check that recurrent events are processed
  expect_gte(length(result$all_cis), 1)

  # Check structure of mcc_base
  expect_s3_class(result$mcc_base, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("time", "cm", "Deta", "cumI") %in% names(result$mcc_base)))

  # Check that MCC increases monotonically
  expect_true(all(diff(result$mcc_final$SumCIs) >= -1e-10)) # Allow for floating point errors
})

test_that("mcc_sci() uses time_precision parameter correctly", {
  # Create dataset with multiple simultaneous events
  df <- data.frame(
    id = c(1, 1, 1),
    time = c(10, 10, 10),
    cause = c(1, 2, 0)
  )

  # Test with different precision values
  result1 <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    time_precision = 1e-3
  )

  result2 <- mcc_sci(
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

test_that("mcc_sci() handles the special case of no events after processing", {
  skip_if_not_installed("mstate")
  skip_if_not_installed("survival")

  # Create a dataset where all events are filtered out due to left truncation
  # This requires a specific setup where events occur before their start times
  # We'll mock this by having all events == 0 or 2
  df <- data.frame(
    id = 1:3,
    time = c(10, 15, 20),
    cause = c(2, 0, 2), # No cause=1 events
    tstart = c(5, 10, 15)
  )

  # Should warn about no events for calculation
  expect_warning(
    result <- mcc_sci(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      tstart_var = "tstart"
    ),
    "Setting sum of cumulative incidence"
  )

  # Check that result structure is as expected
  expect_equal(result$mcc_final$SumCIs, 0)
  expect_equal(nrow(result$sci_table), 1)
})

test_that("mcc_sci() works with snapshot testing for complex cases", {
  # Create a realistic dataset with multiple recurrent events
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 4, 4),
    time = c(5, 10, 15, 7, 12, 8, 6, 14),
    cause = c(1, 1, 0, 1, 2, 0, 1, 1)
  )

  expect_snapshot({
    result <- mcc_sci(
      data = df,
      id_var = "id",
      time_var = "time",
      cause_var = "cause"
    )

    # Print key parts of the result
    cat("mcc_final:\n")
    print(result$mcc_final)

    cat("\nNumber of CI columns in sci_table:\n")
    print(sum(grepl("^CI", names(result$sci_table))))

    cat("\nNumber of all_cis elements:\n")
    print(length(result$all_cis))
  })
})


test_that("mcc_sci() respects include_details=FALSE parameter", {
  # Create a simple dataset
  df <- data.frame(
    id = c(1, 2, 3, 4, 4),
    time = c(5, 8, 12, 10, 15),
    cause = c(1, 0, 2, 1, 1)
  )

  # Run with default include_details=TRUE
  result_detailed <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause"
  )

  # Run with include_details=FALSE
  result_simple <- mcc_sci(
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

test_that("mcc_sci() with include_details=FALSE works with all key scenarios", {
  # 1. With simultaneous events
  df_sim <- data.frame(
    id = c(1, 1, 2, 3),
    time = c(5, 5, 8, 10),
    cause = c(1, 2, 0, 1)
  )

  result_sim <- mcc_sci(
    data = df_sim,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  expect_named(result_sim, "mcc_final")
  expect_true(all(c("time", "SumCIs") %in% names(result_sim$mcc_final)))

  # 2. With left truncation
  skip_if_not_installed("mstate")
  skip_if_not_installed("survival")

  df_trunc <- data.frame(
    id = c(1, 2, 3, 4),
    time = c(10, 15, 20, 25),
    cause = c(1, 2, 1, 0),
    tstart = c(5, 8, 10, 12)
  )

  result_trunc <- mcc_sci(
    data = df_trunc,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    tstart_var = "tstart",
    include_details = FALSE
  )

  expect_named(result_trunc, "mcc_final")
  expect_true(all(c("time", "SumCIs") %in% names(result_trunc$mcc_final)))

  # 3. With no events of interest
  df_no_events <- data.frame(
    id = c(1, 2, 3),
    time = c(5, 10, 15),
    cause = c(0, 2, 0)
  )

  expect_warning(
    result_no_events <- mcc_sci(
      data = df_no_events,
      id_var = "id",
      time_var = "time",
      cause_var = "cause",
      include_details = FALSE
    ),
    "Setting sum of cumulative incidence"
  )

  expect_named(result_no_events, "mcc_final")
  expect_equal(result_no_events$mcc_final$SumCIs, 0)
})

test_that("mcc_sci() with include_details=FALSE provides sufficient data for bootstrapping", {
  # Create dataset with multiple recurrent events
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 3, 4, 4),
    time = c(5, 10, 15, 7, 12, 8, 6, 14),
    cause = c(1, 1, 0, 1, 2, 0, 1, 1)
  )

  # Run with simplified output
  result <- mcc_sci(
    data = df,
    id_var = "id",
    time_var = "time",
    cause_var = "cause",
    include_details = FALSE
  )

  # Verify we have all the time points and MCC values needed for CI calculation
  expect_true("time" %in% names(result$mcc_final))
  expect_true("SumCIs" %in% names(result$mcc_final))

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
    boot_result <- mcc_sci(
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
        return(x$SumCIs[x$time == t1])
      } else {
        return(NA)
      }
    })

    # Verify we can calculate a confidence interval
    expect_true(!all(is.na(mcc_values)))
  }
})
