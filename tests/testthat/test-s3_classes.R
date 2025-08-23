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

  # Test filter_mcc
  filter_result <- filter_mcc(mcc_grouped, "A")
  expect_s3_class(filter_result, "mcc")
  expect_true(all(filter_result$mcc_final$group == "A"))

  expect_error(filter_mcc(mcc_grouped, "C"), "Groups not found")

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
  expect_error(filter_mcc(df, "A"), "must be an.*mcc.*object")
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

# Test as_mcc() function ----
test_that("as_mcc works correctly for different input types", {
  # Test data
  df_data <- data.frame(
    time = c(1, 2, 3, 4),
    mcc = c(0.1, 0.2, 0.3, 0.4)
  )

  df_sci <- data.frame(
    time = c(1, 2, 3, 4),
    SumCIs = c(0.1, 0.2, 0.3, 0.4)
  )

  list_data <- list(
    mcc_final = df_data,
    some_other_component = "test"
  )

  # Test data.frame conversion for equation method
  mcc_from_df <- as_mcc(df_data, method = "equation")
  expect_s3_class(mcc_from_df, "mcc")
  expect_s3_class(mcc_from_df, "mcc_equation")
  # Fix: Compare data.frame to data.frame, not tibble
  expect_equal(as.data.frame(mcc_from_df$mcc_final), df_data)

  # Test data.frame conversion for sci method
  mcc_from_df_sci <- as_mcc(df_sci, method = "sci")
  expect_s3_class(mcc_from_df_sci, "mcc_sci")
  expect_equal(as.data.frame(mcc_from_df_sci$mcc_final), df_sci)

  # Test list conversion
  mcc_from_list <- as_mcc(list_data, method = "equation")
  expect_s3_class(mcc_from_list, "mcc")
  expect_equal(as.data.frame(mcc_from_list$mcc_final), df_data)

  df_with_group <- data.frame(
    time = c(1, 2, 3, 4),
    mcc = c(0.1, 0.2, 0.3, 0.4),
    treatment = c("A", "A", "B", "B")
  )

  list_with_group <- list(
    mcc_final = df_with_group,
    some_other_component = "test"
  )

  mcc_grouped_weighted <- as_mcc(
    list_with_group,
    method = "equation",
    weighted = TRUE,
    by_group = "treatment"
  )
  expect_true(is_weighted(mcc_grouped_weighted))
  expect_equal(mcc_grouping_var(mcc_grouped_weighted), "treatment")
})

test_that("as_mcc error handling works correctly", {
  # Test unsupported class
  expect_error(
    as_mcc("character_string", method = "equation"),
    "Don't know how to convert"
  )

  # Test missing mcc_final in list
  bad_list <- list(other_data = "test")
  expect_error(
    as_mcc(bad_list, method = "equation"),
    "List must contain.*mcc_final"
  )

  # Test missing required columns in data.frame
  bad_df <- data.frame(x = 1, y = 2)
  expect_error(as_mcc(bad_df, method = "equation"), "missing required columns")

  # Test wrong column for sci method
  df_wrong_col <- data.frame(time = 1:3, mcc = 1:3)
  expect_error(
    as_mcc(df_wrong_col, method = "sci"),
    "missing required columns.*SumCIs"
  )
})

# Test utility functions ----
test_that("mcc_final_values works correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0),
    group = c("A", "A", "B", "B", "B", "B", "B")
  )

  # Test ungrouped
  mcc_simple <- mcc(df, "id", "time", "cause")
  final_vals <- mcc_final_values(mcc_simple)

  expect_type(final_vals, "double")
  expect_equal(names(final_vals), "Overall")
  expect_length(final_vals, 1)

  # Test grouped
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  final_vals_grouped <- mcc_final_values(mcc_grouped)

  expect_type(final_vals_grouped, "double")
  expect_equal(sort(names(final_vals_grouped)), c("A", "B"))
  expect_length(final_vals_grouped, 2)
})

test_that("filter_mcc works correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
    time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
    cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
    group = c("A", "A", "B", "B", "B", "B", "B", "C", "C")
  )

  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")

  # Test filtering to single group
  filter_a <- filter_mcc(mcc_grouped, "A")
  expect_s3_class(filter_a, "mcc")
  expect_true(all(filter_a$mcc_final$group == "A"))

  # Test filtering to multiple groups
  filter_ab <- filter_mcc(mcc_grouped, c("A", "B"))
  expect_true(all(filter_ab$mcc_final$group %in% c("A", "B")))
  expect_false(any(filter_ab$mcc_final$group == "C"))

  # Test error for non-existent group
  expect_error(filter_mcc(mcc_grouped, "D"), "Groups not found")

  # Test error for ungrouped MCC
  mcc_simple <- mcc(df[1:3, ], "id", "time", "cause")
  expect_error(filter_mcc(mcc_simple, "A"), "must be a grouped")

  # Test error for non-character groups
  expect_error(filter_mcc(mcc_grouped, 1), "must be a character vector")
})

test_that("compare_mcc works correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0)
  )

  mcc1 <- mcc(df, "id", "time", "cause", method = "equation")
  mcc2 <- mcc(df, "id", "time", "cause", method = "equation")
  mcc3 <- mcc(df, "id", "time", "cause", method = "sci")

  # Test identical objects
  comp_same <- compare_mcc(mcc1, mcc2)
  expect_s3_class(comp_same, "mcc_comparison")
  expect_true(comp_same$methods_match)
  expect_true(comp_same$weighted_match)
  expect_true(comp_same$objects_equivalent)

  # Test different methods
  comp_diff <- compare_mcc(mcc1, mcc3)
  expect_false(comp_diff$methods_match)
  expect_false(comp_diff$objects_equivalent)

  # Test print method
  expect_snapshot(print(comp_same))
  expect_snapshot(print(comp_diff))
})

# Test edge cases and error conditions ----
test_that("S3 methods handle edge cases correctly", {
  # Test with minimal valid data
  df_minimal <- data.frame(
    id = 1,
    time = 1,
    cause = 0
  )

  mcc_minimal <- mcc(df_minimal, "id", "time", "cause")

  # Should work with all utility functions
  expect_type(mcc_method(mcc_minimal), "character")
  expect_type(is_weighted(mcc_minimal), "logical")
  expect_type(is_grouped(mcc_minimal), "logical")
  expect_s3_class(mcc_estimates(mcc_minimal), "data.frame")
  expect_type(mcc_final_values(mcc_minimal), "double")

  # Print and summary should work
  expect_output(print(mcc_minimal))
  expect_snapshot(summary(mcc_minimal))
})

test_that("Error messages are informative", {
  df <- data.frame(x = 1, y = 2)

  # Test various functions with non-MCC input
  expect_error(mcc_method(df), "must be an.*mcc.*object")
  expect_error(is_weighted(df), "must be an.*mcc.*object")
  expect_error(is_grouped(df), "must be an.*mcc.*object")
  expect_error(mcc_grouping_var(df), "must be an.*mcc.*object")
  expect_error(mcc_groups(df), "must be an.*mcc.*object")
  expect_error(mcc_estimates(df), "must be an.*mcc.*object")
  expect_error(mcc_details(df), "must be an.*mcc.*object")
  expect_error(filter_mcc(df, "A"), "must be an.*mcc.*object")
  expect_error(mcc_final_values(df), "must be an.*mcc.*object")
  expect_error(compare_mcc(df, df), "must be.*mcc.*objects")
  expect_error(as.data.frame.mcc(df), "must be an.*mcc.*object")
})

# Test method dispatch and class hierarchy ----
test_that("S3 method dispatch works correctly", {
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0),
    group = c("A", "A", "B", "B", "B", "B", "B"),
    weights = c(1, 1, 1, 1, 1, 1, 1)
  )

  # Test that different class combinations work
  mcc_eq <- mcc(df, "id", "time", "cause")
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
  mcc_weighted <- mcc(df, "id", "time", "cause", weights = "weights")
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  mcc_all <- mcc(df, "id", "time", "cause", by = "group", weights = "weights")

  # Test that print works for all
  expect_output(print(mcc_eq))
  expect_output(print(mcc_sci))
  expect_output(print(mcc_weighted))
  expect_output(print(mcc_grouped))
  expect_output(print(mcc_all))

  # Test that summary works for all
  expect_s3_class(summary(mcc_eq), "summary.mcc")
  expect_s3_class(summary(mcc_sci), "summary.mcc")
  expect_s3_class(summary(mcc_weighted), "summary.mcc")
  expect_s3_class(summary(mcc_grouped), "summary.mcc")
  expect_s3_class(summary(mcc_all), "summary.mcc")

  # Test that as.data.frame works for all
  expect_s3_class(as.data.frame(mcc_eq), "data.frame")
  expect_s3_class(as.data.frame(mcc_sci), "data.frame")
  expect_s3_class(as.data.frame(mcc_weighted), "data.frame")
  expect_s3_class(as.data.frame(mcc_grouped), "data.frame")
  expect_s3_class(as.data.frame(mcc_all), "data.frame")
})

# Test class validation edge cases ----
test_that("validate_mcc catches various invalid objects", {
  # Test object without mcc_final
  bad_obj1 <- list(method = "equation", weighted = FALSE)
  class(bad_obj1) <- "mcc"
  expect_error(validate_mcc(bad_obj1), "missing required components")

  # Test object with non-data.frame mcc_final
  bad_obj2 <- list(
    mcc_final = "not_a_dataframe",
    method = "equation",
    weighted = FALSE
  )
  class(bad_obj2) <- "mcc"
  expect_error(validate_mcc(bad_obj2), "must be a.*data.frame")

  # Test object with mcc_final missing time column
  bad_obj3 <- list(
    mcc_final = data.frame(x = 1, y = 2),
    method = "equation",
    weighted = FALSE
  )
  class(bad_obj3) <- "mcc"
  expect_error(validate_mcc(bad_obj3), "must contain a.*time.*column")

  # Test equation method missing mcc column
  bad_obj4 <- list(
    mcc_final = data.frame(time = 1, other = 2),
    method = "equation",
    weighted = FALSE
  )
  class(bad_obj4) <- "mcc"
  expect_error(validate_mcc(bad_obj4), "must contain an.*mcc.*column")

  # Test sci method missing SumCIs column
  bad_obj5 <- list(
    mcc_final = data.frame(time = 1, mcc = 2),
    method = "sci",
    weighted = FALSE
  )
  class(bad_obj5) <- "mcc"
  expect_error(validate_mcc(bad_obj5), "must contain a.*SumCIs.*column")

  # Test grouped object missing by_group
  bad_obj6 <- list(
    mcc_final = data.frame(time = 1, mcc = 2),
    method = "equation",
    weighted = FALSE
  )
  class(bad_obj6) <- c("mcc_grouped", "mcc")
  expect_error(validate_mcc(bad_obj6), "must have a.*by_group.*component")
})


test_that("as_mcc.data.frame works correctly", {
  # Test conversion from data.frame for equation method
  df_eq <- data.frame(
    time = c(1, 2, 3, 4, 5),
    mcc = c(0.1, 0.3, 0.5, 0.7, 1.0)
  )

  mcc_obj <- as_mcc(df_eq, method = "equation")
  expect_s3_class(mcc_obj, "mcc")
  expect_s3_class(mcc_obj, "mcc_equation")
  expect_equal(mcc_obj$method, "equation")

  # Test conversion from data.frame for sci method
  df_sci <- data.frame(
    time = c(1, 2, 3, 4, 5),
    SumCIs = c(0.1, 0.3, 0.5, 0.7, 1.0)
  )

  mcc_sci_obj <- as_mcc(df_sci, method = "sci")
  expect_s3_class(mcc_sci_obj, "mcc")
  expect_s3_class(mcc_sci_obj, "mcc_sci")
  expect_equal(mcc_sci_obj$method, "sci")
})

test_that("as_mcc.list works correctly", {
  # Test conversion from list
  mcc_list <- list(
    mcc_final = data.frame(
      time = c(1, 2, 3),
      mcc = c(0.2, 0.5, 0.8)
    )
  )

  mcc_from_list <- as_mcc(mcc_list, method = "equation")
  expect_s3_class(mcc_from_list, "mcc")
  expect_equal(mcc_from_list$method, "equation")
})

test_that("as_mcc error handling works correctly", {
  # Test error for unsupported class
  expect_error(
    as_mcc("character_string", method = "equation"),
    "Don't know how to convert"
  )

  # Test error for missing required columns
  bad_df <- data.frame(x = 1, y = 2)
  expect_error(
    as_mcc(bad_df, method = "equation"),
    "missing required columns"
  )

  # Test error for wrong column name in sci method
  wrong_col_df <- data.frame(time = 1:3, mcc = 1:3) # Should be SumCIs for sci
  expect_error(
    as_mcc(wrong_col_df, method = "sci"),
    "missing required columns.*SumCIs"
  )
})

test_that("validate_mcc handles various edge cases", {
  # Test object with invalid mcc_final structure
  bad_obj <- list(
    mcc_final = data.frame(x = 1, y = 2), # Missing time column
    method = "equation",
    weighted = FALSE
  )
  class(bad_obj) <- "mcc"

  expect_error(
    validate_mcc(bad_obj),
    "must contain a.*time.*column"
  )

  # Test object missing method
  bad_obj2 <- list(
    mcc_final = data.frame(time = 1, mcc = 1),
    weighted = FALSE
  )
  class(bad_obj2) <- "mcc"

  expect_error(
    validate_mcc(bad_obj2),
    "missing required components"
  )
})

test_that("mcc_details correctly dispatches based on class", {
  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0) # ID 4 ends with cause = 0 (censoring)
  )

  # Test equation method returns mcc_table
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
  details_eq <- mcc_details(mcc_eq)
  expect_s3_class(details_eq, "data.frame")
  expect_true("mcc" %in% names(details_eq))

  # Test SCI method returns sci_table
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
  details_sci <- mcc_details(mcc_sci)
  expect_s3_class(details_sci, "data.frame")
  expect_true("SumCIs" %in% names(details_sci))

  # Test that different methods return different structures
  expect_false(identical(names(details_eq), names(details_sci)))
})

test_that("create_subtitle function works correctly", {
  # Ensure all participants end with cause = 0 or 2 to avoid warnings
  df <- data.frame(
    id = c(1, 2, 3, 4, 4, 4, 4),
    time = c(8, 1, 5, 2, 6, 7, 8),
    cause = c(0, 0, 2, 1, 1, 1, 0), # ID 4 ends with cause = 0 (censoring)
    group = c("A", "A", "B", "B", "B", "B", "B"),
    weights = c(1, 1, 1, 1, 1, 1, 1)
  )

  # Test subtitle for equation method
  mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
  subtitle_eq <- create_subtitle(mcc_eq)
  expect_true(grepl("Dong-Yasui Equation Method", subtitle_eq))

  # Test subtitle for SCI method
  mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
  subtitle_sci <- create_subtitle(mcc_sci)
  expect_true(grepl("Sum of Cumulative Incidence Method", subtitle_sci))

  # Test subtitle for weighted analysis
  mcc_weighted <- mcc(df, "id", "time", "cause", weights = "weights")
  subtitle_weighted <- create_subtitle(mcc_weighted)
  expect_true(grepl("Weighted", subtitle_weighted))

  # Test subtitle for grouped analysis
  mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
  subtitle_grouped <- create_subtitle(mcc_grouped)
  expect_true(grepl("groups", subtitle_grouped))
})
