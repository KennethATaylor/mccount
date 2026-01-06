# Helper function to create test data
create_test_data <- function(n_groups = 2) {
  if (n_groups == 2) {
    df <- data.frame(
      id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
      time = c(8, 7, 5, 2, 6, 7, 8, 3, 4),
      cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
      group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
    ) |>
      dplyr::arrange(id, time)
  } else if (n_groups == 3) {
    df <- data.frame(
      id = c(1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 8, 8, 9),
      time = c(8, 7, 5, 6, 7, 7, 8, 3, 4, 4, 5, 6, 6),
      cause = c(0, 0, 2, 1, 0, 1, 0, 1, 0, 2, 1, 0, 0),
      group = c("A", "A", "A", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C")
    ) |>
      dplyr::arrange(id, time)
  }

  return(df)
}

# Input Validation Tests --------------------------------------------------

test_that("compare_groups rejects non-mcc objects", {
  df <- data.frame(x = 1, y = 2)

  expect_snapshot(
    compare_groups(df),
    error = TRUE
  )
})

test_that("compare_groups rejects ungrouped mcc objects", {
  df <- create_test_data(2)
  mcc_ungrouped <- mcc(df, "id", "time", "cause")

  expect_snapshot(
    compare_groups(mcc_ungrouped),
    error = TRUE
  )

  expect_snapshot(
    compare_groups(mcc_ungrouped),
    error = TRUE
  )
})

test_that("compare_groups rejects invalid reference groups", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Non-existent reference
  expect_snapshot(
    compare_groups(mcc_grouped, reference = "Z"),
    error = TRUE
  )
})

test_that("compare_groups rejects multiple references when pairwise = FALSE", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  expect_snapshot(
    compare_groups(mcc_grouped, reference = c("A", "B"), pairwise = FALSE),
    error = TRUE
  )

  expect_snapshot(
    compare_groups(mcc_grouped, reference = c("A", "B"), pairwise = FALSE),
    error = TRUE
  )
})

test_that("compare_groups rejects invalid reference preferences in pairwise mode", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  expect_snapshot(
    compare_groups(mcc_grouped, reference = c("A", "Z"), pairwise = TRUE),
    error = TRUE
  )
})

test_that("compare_groups validates measure argument", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  expect_snapshot(
    compare_groups(mcc_grouped, measure = "invalid"),
    error = TRUE
  )
})

# Two-Group Comparison Tests ----------------------------------------------

test_that("compare_groups works with 2 groups using default reference", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Should use "A" as reference (first alphabetically)
  expect_snapshot(
    result <- compare_groups(mcc_grouped)
  )

  expect_s3_class(result, "mcc_group_comparison")
  expect_equal(length(result$comparisons), 1)
  expect_equal(result$metadata$reference_group, "A")
  expect_false(result$metadata$pairwise)
})

test_that("compare_groups calculates MCCD correctly", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "A", measure = "difference")

  # Check structure
  expect_equal(result$metadata$measure, "difference")
  comparison_df <- result$comparisons[[1]]

  expect_true("mccd" %in% names(comparison_df))
  expect_true("mcc_B" %in% names(comparison_df))
  expect_true("mcc_A" %in% names(comparison_df))
  expect_false("mccr" %in% names(comparison_df))

  # Check calculation: mccd = mcc_B - mcc_A
  expect_equal(
    comparison_df$mccd,
    comparison_df$mcc_B - comparison_df$mcc_A
  )
})

test_that("compare_groups calculates MCCR correctly", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "B", measure = "ratio")

  # Check structure
  expect_equal(result$metadata$measure, "ratio")
  comparison_df <- result$comparisons[[1]]

  expect_true("mccr" %in% names(comparison_df))
  expect_false("mccd" %in% names(comparison_df))

  # Check calculation: mccr = mcc_A / mcc_B
  expect_equal(
    comparison_df$mccr,
    # added small increment so that ratio when both = 0 is 1.0
    (comparison_df$mcc_A + 0.000000001) / (comparison_df$mcc_B + 0.000000001)
  )
})

test_that("compare_groups calculates both measures when requested", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "B", measure = "both")

  expect_equal(result$metadata$measure, "both")
  comparison_df <- result$comparisons[[1]]

  expect_true("mccd" %in% names(comparison_df))
  expect_true("mccr" %in% names(comparison_df))
})

test_that("compare_groups handles specified reference group", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "B")

  expect_equal(result$metadata$reference_group, "B")
  comparison_df <- result$comparisons[[1]]

  expect_equal(comparison_df$reference[1], "B")
  expect_equal(comparison_df$comparison[1], "A")
})

# Multi-Group Comparison Tests --------------------------------------------

test_that("compare_groups handles 3+ groups with default reference", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Should warn about multiple groups and default selection
  expect_snapshot(
    result <- compare_groups(mcc_grouped)
  )

  expect_equal(result$metadata$reference_group, "A")
  expect_equal(length(result$comparisons), 2) # B vs A, C vs A
})

test_that("compare_groups handles 3+ groups with specified reference", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "B")

  expect_equal(result$metadata$reference_group, "B")
  expect_equal(length(result$comparisons), 2) # A vs B, C vs B

  # Check that all comparisons use B as reference
  refs <- sapply(result$comparisons, function(x) unique(x$reference))
  expect_true(all(refs == "B"))
})

# Pairwise Comparison Tests -----------------------------------------------

test_that("compare_groups performs all pairwise comparisons", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  expect_snapshot(
    result <- compare_groups(mcc_grouped, pairwise = TRUE)
  )

  expect_true(result$metadata$pairwise)
  expect_equal(length(result$comparisons), 3) # A-B, A-C, B-C

  # Check that we have all expected pairs
  pairs <- result$metadata$comparison_pairs
  all_pairs <- paste(pairs$comparison, pairs$reference, sep = "-")

  expect_equal(length(unique(all_pairs)), 3)
})

test_that("compare_groups uses reference preferences in pairwise mode", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Prefer A as reference, then B
  result <- compare_groups(
    mcc_grouped,
    reference = c("A", "B"),
    pairwise = TRUE
  )

  pairs <- result$metadata$comparison_pairs

  # For pairs containing A, A should be reference
  a_pairs <- pairs[pairs$reference == "A" | pairs$comparison == "A", ]
  expect_true(all(a_pairs$reference == "A"))

  # For B-C pair (no A), B should be reference
  bc_pair <- pairs[pairs$reference == "B" & pairs$comparison == "C", ]
  expect_equal(nrow(bc_pair), 1)
})

test_that("compare_groups warns about default reference in pairwise mode", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Only specify A as preference - B-C comparison will use default
  expect_warning(
    result <- compare_groups(mcc_grouped, reference = "A", pairwise = TRUE)
  )

  expect_warning(
    result <- compare_groups(mcc_grouped, reference = "A", pairwise = TRUE),
    "alphabetical default"
  )

  # Check that one comparison used default
  pairs <- result$metadata$comparison_pairs
  expect_true(any(pairs$default_used))
})

test_that("pairwise comparisons work with all groups in preference vector", {
  df <- create_test_data(3)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # All groups in preference order - no defaults should be used
  result <- compare_groups(
    mcc_grouped,
    reference = c("C", "B", "A"),
    pairwise = TRUE
  )

  pairs <- result$metadata$comparison_pairs
  expect_false(any(pairs$default_used))

  # C should be preferred over B and A
  # B should be preferred over A
  expect_true("C" %in% pairs$reference)
})

# Time Alignment Tests ----------------------------------------------------

test_that("compare_groups handles different max follow-up times", {
  # Create data with different follow-up by group
  df <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    time = c(3, 5, 5, 6, 3, 10, 2, 8, 1, 8), # Group B has longer follow-up
    cause = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 2),
    group = c("A", "A", "A", "A", "B", "B", "B", "B", "B", "B")
  ) |>
    dplyr::arrange(id, time)

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Should inform about truncation
  expect_message(
    result <- compare_groups(mcc_grouped, reference = "A"),
    "Valid comparison period"
  )

  # Check truncation time in metadata
  pairs <- result$metadata$comparison_pairs
  expect_equal(pairs$truncation_time, 6) # min(6, 10)

  # Check that no time points exceed truncation
  comparison_df <- result$comparisons[[1]]
  expect_true(all(comparison_df$time <= 10))
})

test_that("compare_groups uses LOCF for misaligned times", {
  # Create data with different event times
  df <- data.frame(
    id = c(1, 1, 2, 3, 3, 4),
    time = c(1, 3, 5, 2, 5, 5), # Different time points
    cause = c(1, 0, 0, 1, 0, 0),
    group = c("A", "A", "A", "B", "B", "B")
  ) |>
    dplyr::arrange(id, time)

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  result <- compare_groups(mcc_grouped, reference = "A")

  comparison_df <- result$comparisons[[1]]

  # Should have times from both groups (union)
  expect_true(1 %in% comparison_df$time)
  expect_true(2 %in% comparison_df$time)
  expect_true(5 %in% comparison_df$time)

  # MCC values should be carried forward (non-decreasing within each group)
  a_values <- comparison_df |> dplyr::pull("mcc_A")
  b_values <- comparison_df |> dplyr::pull("mcc_A")

  expect_true(all(diff(a_values) >= 0))
  expect_true(all(diff(b_values) >= 0))
})

# Edge Cases and Special Situations --------------------------------------

test_that("compare_groups handles division by zero in MCCR", {
  # Create data where reference has MCC = 0 at some times
  df <- data.frame(
    id = c(1, 2, 2, 3),
    time = c(1, 1, 2, 5),
    cause = c(0, 1, 0, 0),
    group = c("A", "B", "B", "B")
  ) |>
    dplyr::arrange(id, time)

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Should warn about division by zero (only when comparison group != 0)
  expect_snapshot(
    result <- compare_groups(mcc_grouped, reference = "A", measure = "ratio")
  )

  comparison_df <- result$comparisons[[1]]

  # At time 0, both groups have MCC = 0, so MCCR should be 1.0
  time_0 <- comparison_df[comparison_df$time == 0, ]
  expect_equal(time_0$mccr, 1.0)

  # At later times where reference = 0 but comparison != 0, should be NA
  expect_true(any(is.na(comparison_df$mccr)))
})

test_that("compare_groups works with weighted mcc objects", {
  df <- create_test_data(2)
  df$weights <- c(1.2, 0.8, 1.5, 1.0, 1.0, 1.0, 1.0, 1.3, 1.3)

  mcc_weighted <- mcc(
    df,
    "id",
    "time",
    "cause",
    by = "group",
    weights = "weights"
  )

  result <- compare_groups(mcc_weighted, reference = "A")

  expect_s3_class(result, "mcc_group_comparison")
  expect_true(result$metadata$weighted)
  expect_equal(length(result$comparisons), 1)
})

test_that("compare_groups works with both equation and sci methods", {
  df <- create_test_data(2)

  # Equation method
  mcc_eq <- mcc(df, "id", "time", "cause", by = "group", method = "equation")
  result_eq <- compare_groups(mcc_eq, reference = "A")

  expect_equal(result_eq$metadata$method, "equation")

  # SCI method
  mcc_sci <- mcc(df, "id", "time", "cause", by = "group", method = "sci")
  result_sci <- compare_groups(mcc_sci, reference = "A")

  expect_equal(result_sci$metadata$method, "sci")
})

# Output Structure Tests --------------------------------------------------

test_that("compare_groups returns correct output structure", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "A")

  # Check class
  expect_s3_class(result, "mcc_group_comparison")
  expect_true(is.list(result))

  # Check main components
  expect_true("comparisons" %in% names(result))
  expect_true("metadata" %in% names(result))
  expect_true("original_mcc" %in% names(result))
  expect_true("call" %in% names(result))

  # Check metadata components
  metadata <- result$metadata
  expect_true("reference_group" %in% names(metadata))
  expect_true("pairwise" %in% names(metadata))
  expect_true("measure" %in% names(metadata))
  expect_true("n_comparisons" %in% names(metadata))
  expect_true("comparison_pairs" %in% names(metadata))
  expect_true("weighted" %in% names(metadata))
  expect_true("method" %in% names(metadata))
  expect_true("grouping_var" %in% names(metadata))

  # Check comparison_pairs dataframe
  pairs <- metadata$comparison_pairs
  expect_s3_class(pairs, "data.frame")
  expect_true("reference" %in% names(pairs))
  expect_true("comparison" %in% names(pairs))
  expect_true("truncation_time" %in% names(pairs))
  expect_true("default_used" %in% names(pairs))
})

test_that("comparison dataframes have correct structure", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test with both measures
  result <- compare_groups(mcc_grouped, reference = "B", measure = "both")
  comparison_df <- result$comparisons[[1]]

  expect_s3_class(comparison_df, "data.frame")
  expect_true("time" %in% names(comparison_df))
  expect_true("reference" %in% names(comparison_df))
  expect_true("comparison" %in% names(comparison_df))
  expect_true("mccd" %in% names(comparison_df))
  expect_true("mccr" %in% names(comparison_df))

  # MCC columns should be named with group names
  expect_true(any(grepl("mcc_A", names(comparison_df))))
  expect_true(any(grepl("mcc_B", names(comparison_df))))
})

test_that("original_mcc is preserved in output", {
  df <- create_test_data(2)
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  result <- compare_groups(mcc_grouped, reference = "A")

  expect_identical(result$original_mcc, mcc_grouped)
})
