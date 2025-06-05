# tests/testthat/test-s3-classes.R

test_that("S3 object creation works correctly", {
  # Create test data
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
  )

  # Test equation method
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")

  expect_s3_class(mcc_eq, "mcc")
  expect_s3_class(mcc_eq, "mcc_equation")
  expect_true(is_mcc(mcc_eq))
  expect_equal(mcc_method(mcc_eq), "equation")
  expect_false(is_weighted(mcc_eq))
  expect_false(is_grouped(mcc_eq))

  # Test SCI method
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")

  expect_s3_class(mcc_sci, "mcc")
  expect_s3_class(mcc_sci, "mcc_sci")
  expect_true(is_mcc(mcc_sci))
  expect_equal(mcc_method(mcc_sci), "sci")
  expect_false(is_weighted(mcc_sci))
  expect_false(is_grouped(mcc_sci))
})

test_that("Grouped S3 objects work correctly", {
  # Create test data with groups
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  expect_s3_class(mcc_grouped, "mcc")
  expect_s3_class(mcc_grouped, "mcc_grouped")
  expect_true(is_grouped(mcc_grouped))
  expect_equal(mcc_grouping_var(mcc_grouped), "group")
  expect_equal(sort(mcc_groups(mcc_grouped)), c("A", "B"))

  # Test group column exists in results
  expect_true("group" %in% names(mcc_grouped$mcc_final))
})

test_that("Weighted S3 objects work correctly", {
  # Create test data with weights
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    weights = c(1, 1.5, 0.8, 1.2, 1.2, 1.2, 1.2, 2.0, 2.0)
  )

  mcc_weighted <- mcc(df, "id", "time", "cause", weights = "weights")

  expect_s3_class(mcc_weighted, "mcc")
  expect_s3_class(mcc_weighted, "mcc_weighted")
  expect_true(is_weighted(mcc_weighted))
})

test_that("S3 validation works correctly", {
  # Test invalid objects
  invalid_obj <- list(method = "equation")
  class(invalid_obj) <- "mcc"

  expect_error(validate_mcc(invalid_obj), "missing required components")

  # Test is_mcc with non-MCC objects
  expect_false(is_mcc(data.frame(x = 1)))
  expect_false(is_mcc("not an mcc"))
  expect_false(is_mcc(NULL))
})

test_that("Print methods work without errors", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test print method output with snapshot
  expect_snapshot({
    print(mcc_result)
  })

  # Test that print returns the object invisibly
  expect_identical(print(mcc_result), mcc_result)

  # Test summary print method
  summary_obj <- summary(mcc_result)
  expect_snapshot({
    print(summary_obj)
  })

  # Test that summary print returns the object invisibly
  expect_identical(print(summary_obj), summary_obj)
})

test_that("Print methods work for grouped objects", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test grouped print output
  expect_snapshot({
    print(mcc_grouped)
  })

  # Test grouped summary output
  expect_snapshot({
    print(summary(mcc_grouped))
  })
})

test_that("Print methods work for weighted objects", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    weights = c(1, 1.5, 0.8, 1.2, 1.2, 1.2, 1.2, 2.0, 2.0)
  )

  mcc_weighted <- mcc(df, "id", "time", "cause", weights = "weights")

  # Test weighted print output
  expect_snapshot({
    print(mcc_weighted)
  })
})


test_that("Utility functions work correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0)
  )

  mcc_result <- mcc(df, "id", "time", "cause")

  # Test mcc_estimates
  estimates <- mcc_estimates(mcc_result)
  expect_s3_class(estimates, "data.frame")
  expect_true("time" %in% names(estimates))
  expect_true("mcc" %in% names(estimates))

  # Test mcc_details
  details <- mcc_details(mcc_result)
  expect_s3_class(details, "data.frame")

  # Test as.data.frame
  df_result <- as.data.frame(mcc_result)
  expect_s3_class(df_result, "data.frame")
  expect_equal(df_result, as.data.frame(estimates))

  # Test mcc_final_values
  final_vals <- mcc_final_values(mcc_result)
  expect_type(final_vals, "double")
  expect_equal(names(final_vals), "Overall")
})

test_that("Grouped utility functions work correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test subset_mcc
  subset_result <- subset_mcc(mcc_grouped, "A")
  expect_s3_class(subset_result, "mcc")
  expect_true(all(subset_result$mcc_final$group == "A"))

  expect_error(subset_mcc(mcc_grouped, "C"), "Groups not found")

  # Test mcc_final_values for grouped
  final_vals <- mcc_final_values(mcc_grouped)
  expect_equal(length(final_vals), 2)
  expect_equal(sort(names(final_vals)), c("A", "B"))
})

test_that("MCC comparison print works correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0)
  )

  mcc1 <- mcc(df, "id", "time", "cause", method = "equation")
  mcc2 <- mcc(df, "id", "time", "cause", method = "equation")
  mcc3 <- mcc(df, "id", "time", "cause", method = "sci")

  # Same objects should be equivalent
  comp1 <- compare_mcc(mcc1, mcc2)
  expect_s3_class(comp1, "mcc_comparison")
  expect_true(comp1$objects_equivalent)

  # Test comparison print output
  expect_snapshot({
    print(comp1)
  })

  # Test that print returns the object invisibly
  expect_identical(print(comp1), comp1)

  # Different methods should not be equivalent
  comp2 <- compare_mcc(mcc1, mcc3)
  expect_false(comp2$methods_match)
  expect_false(comp2$objects_equivalent)

  # Test different comparison output
  expect_snapshot({
    print(comp2)
  })
})

test_that("Error handling works correctly", {
  df <- data.frame(x = 1, y = 2)

  # Test errors for non-MCC objects
  expect_error(mcc_method(df), "must be an.*mcc.*object")
  expect_error(is_weighted(df), "must be an.*mcc.*object")
  expect_error(is_grouped(df), "must be an.*mcc.*object")
  expect_error(mcc_estimates(df), "must be an.*mcc.*object")
  expect_error(subset_mcc(df, "A"), "must be an.*mcc.*object")
  expect_error(compare_mcc(df, df), "must be.*mcc.*objects")
})

test_that("S3 classes are assigned correctly for all combinations", {
  # Create data with events of interest (cause = 1) to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5, 6, 6),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 4, 4, 5),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 0, 1, 0),
    group = c("A", "A", "B", "B", "B", "B", "B", "A", "A", "A", "A"),
    weights = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )

  # Test all combinations
  mcc_eq_simple <- mcc(df, "id", "time", "cause", method = "equation")
  expect_equal(class(mcc_eq_simple), c("mcc_equation", "mcc"))

  mcc_eq_weighted <- mcc(
    df,
    "id",
    "time",
    "cause",
    method = "equation",
    weights = "weights"
  )
  expect_equal(class(mcc_eq_weighted), c("mcc_weighted", "mcc_equation", "mcc"))

  mcc_eq_grouped <- mcc(
    df,
    "id",
    "time",
    "cause",
    method = "equation",
    by = "group"
  )
  expect_equal(class(mcc_eq_grouped), c("mcc_grouped", "mcc_equation", "mcc"))

  mcc_eq_both <- mcc(
    df,
    "id",
    "time",
    "cause",
    method = "equation",
    by = "group",
    weights = "weights"
  )
  expect_equal(
    class(mcc_eq_both),
    c("mcc_grouped", "mcc_weighted", "mcc_equation", "mcc")
  )

  mcc_sci_simple <- mcc(df, "id", "time", "cause", method = "sci")
  expect_equal(class(mcc_sci_simple), c("mcc_sci", "mcc"))

  mcc_sci_grouped <- mcc(
    df,
    "id",
    "time",
    "cause",
    method = "sci",
    by = "group"
  )
  expect_equal(class(mcc_sci_grouped), c("mcc_grouped", "mcc_sci", "mcc"))
})

# Additional test for as_mcc functionality
test_that("as_mcc conversion works correctly", {
  # Test converting data.frame to MCC
  df_data <- data.frame(
    time = c(1, 2, 3, 4),
    mcc = c(0.1, 0.25, 0.4, 0.6)
  )

  mcc_from_df <- as_mcc(df_data, method = "equation")
  expect_s3_class(mcc_from_df, "mcc")
  expect_s3_class(mcc_from_df, "mcc_equation")
  expect_equal(mcc_method(mcc_from_df), "equation")

  # Test converting list to MCC
  list_data <- list(
    mcc_final = data.frame(time = c(1, 2, 3), SumCIs = c(0.2, 0.4, 0.7)),
    additional_component = "extra data"
  )

  mcc_from_list <- as_mcc(list_data, method = "sci")
  expect_s3_class(mcc_from_list, "mcc")
  expect_s3_class(mcc_from_list, "mcc_sci")
  expect_equal(mcc_method(mcc_from_list), "sci")

  # Test error for unsupported class
  expect_error(
    as_mcc(matrix(1:4, ncol = 2), method = "equation"),
    "Don't know how to convert"
  )

  # Test error for missing columns
  bad_df <- data.frame(time = 1:3, wrong_col = 1:3)
  expect_error(as_mcc(bad_df, method = "equation"), "missing required columns")
})
