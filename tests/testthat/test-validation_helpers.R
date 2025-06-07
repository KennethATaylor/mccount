test_that("validate_column_existence() works", {
  expect_snapshot(
    error = TRUE,
    validate_column_existence(
      data = data.frame(a = 1),
      id_var = "patid",
      time_var = "survtime",
      cause_var = "status"
    )
  )

  expect_snapshot(
    error = TRUE,
    validate_column_existence(
      data = data.frame(a = 1),
      id_var = "patid",
      time_var = "survtime",
      cause_var = "status",
      tstart_var = "tstart"
    )
  )

  expect_no_error(
    validate_column_existence(
      data = data.frame(patid = 1, survtime = 1, status = 1),
      id_var = "patid",
      time_var = "survtime",
      cause_var = "status"
    )
  )

  expect_no_error(
    validate_column_existence(
      data = data.frame(patid = 1, survtime = 1, status = 1, tstart = 1),
      id_var = "patid",
      time_var = "survtime",
      cause_var = "status",
      tstart_var = "tstart"
    )
  )
})


test_that("validate_data_type() works", {
  expect_snapshot(error = TRUE, validate_data_type(data = "not a data.frame"))
})


test_that("validate_cause_values() works", {
  expect_snapshot(
    error = TRUE,
    validate_cause_values(data.frame(x = -1), quote(x))
  )
})


test_that("validate_time_tstart() works", {
  expect_snapshot(
    error = TRUE,
    validate_time_tstart(
      data = data.frame(t = 1:3, tstart = 2:4),
      time_var = "t",
      tstart_var = "tstart"
    )
  )
})


test_that("standardize_data() works", {
  expect_snapshot(
    standardize_data(
      data = mtcars,
      id_var = 1,
      time_var = "mpg",
      cause_var = "cyl",
      tstart_var = "wt"
    )
  )

  expect_snapshot(
    error = TRUE,
    standardize_data(
      data = mtcars,
      id_var = "carb",
      time_var = NULL,
      cause_var = "cyl"
    )
  )

  expect_snapshot(
    error = TRUE,
    standardize_data(
      data = NULL,
      time_var = "mpg",
      id_var = "cyl",
      cause_var = "disp"
    )
  )
})

test_that("validate_data_type() works with various data types", {
  # Test with valid data types
  expect_no_error(validate_data_type(data.frame(x = 1)))
  expect_no_error(validate_data_type(tibble::tibble(x = 1)))

  # Test with invalid data types
  expect_snapshot(error = TRUE, validate_data_type(list(x = 1)))
  expect_snapshot(error = TRUE, validate_data_type(matrix(1:4, nrow = 2)))
  expect_snapshot(error = TRUE, validate_data_type(c(1, 2, 3)))
  expect_snapshot(error = TRUE, validate_data_type(NULL))
})

test_that("validate_cause_values() works with valid values", {
  # Test with valid cause values
  expect_no_error(validate_cause_values(
    data.frame(cause = c(0, 1, 2)),
    quote(cause)
  ))
  expect_no_error(validate_cause_values(
    data.frame(cause = c(1, 1, 1)),
    quote(cause)
  ))
  expect_no_error(validate_cause_values(data.frame(cause = 0), quote(cause)))
  expect_no_error(validate_cause_values(data.frame(cause = 2), quote(cause)))

  # Test with multiple invalid values
  expect_snapshot(
    error = TRUE,
    validate_cause_values(data.frame(status = c(-1, 3, 5)), quote(status))
  )

  # Test with mixed valid and invalid values
  expect_snapshot(
    error = TRUE,
    validate_cause_values(
      data.frame(outcome = c(0, 1, 3, 2, -1)),
      quote(outcome)
    )
  )
})

test_that("validate_time_tstart() works with valid values", {
  # Test with valid time > tstart
  expect_no_error(
    validate_time_tstart(
      data = data.frame(time = c(5, 10, 15), tstart = c(0, 2, 8)),
      time_var = "time",
      tstart_var = "tstart"
    )
  )

  # Test with single valid pair
  expect_no_error(
    validate_time_tstart(
      data = data.frame(end_time = 10, start_time = 5),
      time_var = "end_time",
      tstart_var = "start_time"
    )
  )

  # Test with NA values (should be ignored)
  expect_no_error(
    validate_time_tstart(
      data = data.frame(time = c(5, NA, 15), tstart = c(0, 2, NA)),
      time_var = "time",
      tstart_var = "tstart"
    )
  )

  # Test with equal times (should fail)
  expect_snapshot(
    error = TRUE,
    validate_time_tstart(
      data = data.frame(time = c(5, 5), tstart = c(5, 3)),
      time_var = "time",
      tstart_var = "tstart"
    )
  )
})

test_that("validate_by_variable() works", {
  # Test with NULL (should pass)
  expect_no_error(validate_by_variable(data.frame(x = 1), NULL))

  # Test with valid by variable
  expect_no_error(
    validate_by_variable(
      data.frame(group = c("A", "B", "A")),
      "group"
    )
  )

  # Test with non-character by variable
  expect_snapshot(
    error = TRUE,
    validate_by_variable(data.frame(x = 1), 123)
  )

  # Test with multiple values in by
  expect_snapshot(
    error = TRUE,
    validate_by_variable(data.frame(x = 1), c("group1", "group2"))
  )

  # Test with missing column
  expect_snapshot(
    error = TRUE,
    validate_by_variable(data.frame(x = 1), "missing_col")
  )

  # Test with all NA values
  expect_snapshot(
    error = TRUE,
    validate_by_variable(
      data.frame(group = c(NA, NA, NA)),
      "group"
    )
  )

  # Test with many groups (should warn)
  many_groups_data <- data.frame(
    group = paste0("group_", 1:25)
  )
  expect_snapshot(
    validate_by_variable(many_groups_data, "group")
  )

  # Test with some NA values mixed with valid values (should pass)
  expect_no_error(
    validate_by_variable(
      data.frame(treatment = c("A", "B", NA, "A")),
      "treatment"
    )
  )
})

test_that("standardize_data() works with symbol inputs", {
  # Test with symbols instead of strings
  expect_no_error(
    standardize_data(
      data = data.frame(patient = 1:3, followup = 4:6, event = 7:9),
      id_var = rlang::sym("patient"),
      time_var = rlang::sym("followup"),
      cause_var = rlang::sym("event")
    )
  )

  # Test with tstart_var as symbol
  result_with_tstart <- standardize_data(
    data = data.frame(id = 1:2, start = 0:1, stop = 5:6, status = 1:2),
    id_var = rlang::sym("id"),
    time_var = rlang::sym("stop"),
    cause_var = rlang::sym("status"),
    tstart_var = rlang::sym("start")
  )

  expect_true("tstart" %in% names(result_with_tstart))
  expect_equal(result_with_tstart$tstart, c(0, 1))

  # Test column order
  expect_equal(names(result_with_tstart), c("id", "tstart", "time", "cause"))
})

test_that("standardize_data() column ordering and values", {
  # Test that columns are properly ordered when tstart is added
  result_no_tstart <- standardize_data(
    data = data.frame(patient_id = 1:3, time_to_event = 4:6, outcome = 7:9),
    id_var = rlang::sym("patient_id"),
    time_var = rlang::sym("time_to_event"),
    cause_var = rlang::sym("outcome")
  )

  expect_equal(names(result_no_tstart), c("id", "tstart", "time", "cause"))
  expect_equal(result_no_tstart$tstart, c(0, 0, 0))
  expect_equal(result_no_tstart$id, 1:3)
  expect_equal(result_no_tstart$time, 4:6)
  expect_equal(result_no_tstart$cause, 7:9)
})

test_that("validate_last_observation() works with no problematic cases", {
  # Data where all last observations are censoring or competing risks
  data_std <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    tstart = c(0, 0, 0, 0, 0, 0),
    time = c(5, 10, 8, 12, 6, 15),
    cause = c(1, 0, 1, 2, 1, 0) # Last obs for each ID: 0, 2, 0
  )

  expect_no_warning(
    result <- validate_last_observation(data_std)
  )

  expect_true(result)
})

test_that("validate_last_observation() warns for single problematic case", {
  # Data where one participant's last observation is an event
  data_std <- data.frame(
    id = c(1, 1, 2, 2, 3),
    tstart = c(0, 0, 0, 0, 0),
    time = c(5, 10, 8, 12, 15),
    cause = c(1, 0, 1, 1, 2) # ID 2's last obs is cause = 1
  )

  expect_warning(
    result <- validate_last_observation(data_std),
    "Found 1.*where last observation is an event of interest"
  )

  expect_true(result)
})

test_that("validate_last_observation() warns for multiple problematic cases", {
  # Data where multiple participants' last observations are events
  data_std <- data.frame(
    id = c(1, 1, 2, 3, 4, 5),
    tstart = c(0, 0, 0, 0, 0, 0),
    time = c(5, 10, 8, 15, 12, 20),
    cause = c(1, 1, 1, 1, 0, 2) # IDs 1, 2, 3 have last obs as cause = 1
  )

  expect_warning(
    result <- validate_last_observation(data_std),
    "Found 3.*where last observation is an event of interest"
  )

  expect_true(result)
})

test_that("validate_last_observation() handles many problematic cases", {
  # Data with many participants having problematic last observations
  data_std <- data.frame(
    id = 1:10,
    tstart = rep(0, 10),
    time = 1:10,
    cause = c(1, 1, 1, 1, 1, 1, 1, 0, 2, 0) # 7 participants with cause = 1
  )

  expect_warning(
    result <- validate_last_observation(data_std),
    "Found 7.*where last observation is an event of interest"
  )

  # Should mention "First 5 IDs" when more than 5
  expect_warning(
    validate_last_observation(data_std),
    "First 5 IDs"
  )

  expect_true(result)
})

test_that("validate_last_observation() works with unsorted data", {
  # Data not sorted by time
  data_std <- data.frame(
    id = c(1, 1, 1, 2, 2),
    tstart = c(0, 0, 0, 0, 0),
    time = c(10, 5, 15, 8, 3), # Unsorted times
    cause = c(0, 1, 1, 2, 1) # ID 1's actual last obs (time=15) is cause=1
  )

  expect_warning(
    result <- validate_last_observation(data_std),
    "Found 1.*where last observation is an event of interest"
  )

  expect_true(result)
})

test_that("validate_last_observation() works with single observation per ID", {
  # Test where each participant has only one observation
  data_std <- data.frame(
    id = c(1, 2, 3, 4),
    tstart = c(0, 0, 0, 0),
    time = c(5, 8, 12, 15),
    cause = c(1, 2, 0, 1) # IDs 1 and 4 have single obs with cause = 1
  )

  expect_warning(
    result <- validate_last_observation(data_std),
    "Found 2.*where last observation is an event of interest"
  )

  expect_true(result)
})

test_that("validate_last_observation() handles empty data", {
  # Test with empty data frame
  data_std <- data.frame(
    id = numeric(0),
    tstart = numeric(0),
    time = numeric(0),
    cause = numeric(0)
  )

  expect_no_warning(
    result <- validate_last_observation(data_std)
  )

  expect_true(result)
})

test_that("validate_last_observation() handles duplicate times correctly", {
  # Test with participants having multiple observations at the same time
  # (the last chronological observation should be checked)
  data_std <- data.frame(
    id = c(1, 1, 1, 2, 2),
    tstart = c(0, 0, 0, 0, 0),
    time = c(5, 10, 10, 8, 8), # Duplicate times
    cause = c(1, 1, 0, 1, 2) # For ties, should check the last row
  )

  # Since we're checking the last row for each ID after sorting,
  # ID 1's last observation is cause = 0, ID 2's is cause = 2
  expect_no_warning(
    result <- validate_last_observation(data_std)
  )

  expect_true(result)
})

test_that("validate_last_observation() warning message content", {
  # Test the specific content of warning messages
  data_std <- data.frame(
    id = c(1, 2, 3),
    tstart = c(0, 0, 0),
    time = c(5, 8, 12),
    cause = c(1, 1, 0) # IDs 1 and 2 have problematic last obs
  )

  expect_warning(
    validate_last_observation(data_std),
    "assumes"
  )
})

# Test validate_weights_variable function
test_that("validate_weights_variable works correctly", {
  # Create test data
  test_data <- data.frame(
    id = 1:5,
    time = c(1, 2, 3, 4, 5),
    cause = c(1, 0, 2, 1, 0),
    good_weights = c(1.0, 2.0, 0.5, 1.5, 1.2),
    zero_weights = c(1.0, 0.0, 0.5, 1.5, 1.2),
    negative_weights = c(1.0, -0.5, 0.5, 1.5, 1.2),
    missing_weights = c(1.0, NA, 0.5, 1.5, 1.2),
    character_weights = c("1.0", "2.0", "0.5", "1.5", "1.2"),
    extreme_weights = c(1.0, 100.0, 0.01, 1.5, 1.2)
  )

  # Test with NULL weights (should return TRUE)
  expect_true(validate_weights_variable(test_data, NULL))

  # Test with valid weights
  expect_true(validate_weights_variable(test_data, "good_weights"))

  # Test with non-character weights argument
  expect_error(
    validate_weights_variable(test_data, 123),
    class = "rlang_error"
  )
  expect_error(
    validate_weights_variable(test_data, c("weight1", "weight2")),
    class = "rlang_error"
  )

  # Test with non-existent column
  expect_error(
    validate_weights_variable(test_data, "nonexistent_weights"),
    class = "rlang_error"
  )

  # Test with missing values
  expect_error(
    validate_weights_variable(test_data, "missing_weights"),
    class = "rlang_error"
  )

  # Test with non-numeric values
  expect_error(
    validate_weights_variable(test_data, "character_weights"),
    class = "rlang_error"
  )

  # Test with negative values
  expect_error(
    validate_weights_variable(test_data, "negative_weights"),
    class = "rlang_error"
  )

  # Test with zero weights (should warn but not error)
  expect_warning(
    validate_weights_variable(test_data, "zero_weights"),
    class = "rlang_warning"
  )

  # Test with extreme weights (should warn but not error)
  expect_warning(
    validate_weights_variable(test_data, "extreme_weights"),
    class = "rlang_warning"
  )
})

test_that("validate_weights_variable error messages are informative", {
  test_data <- data.frame(
    id = 1:3,
    weights_with_na = c(1.0, NA, 1.5),
    negative_weights = c(1.0, -0.5, 1.5),
    character_weights = c("a", "b", "c")
  )

  # Test missing values error message
  expect_error(
    validate_weights_variable(test_data, "weights_with_na"),
    "Missing values found"
  )

  # Test negative values error message
  expect_error(
    validate_weights_variable(test_data, "negative_weights"),
    "Negative values found"
  )

  # Test non-numeric error message
  expect_error(
    validate_weights_variable(test_data, "character_weights"),
    "must contain numeric values"
  )

  # Test non-existent column error message
  expect_error(
    validate_weights_variable(test_data, "missing_col"),
    "not found in"
  )
})

# Test updated validate_column_existence function with weights
test_that("validate_column_existence works with weights parameter", {
  test_data <- data.frame(
    id = 1:3,
    time = c(1, 2, 3),
    cause = c(1, 0, 2),
    weights = c(1.0, 1.5, 0.8)
  )

  # Test with valid weights column
  expect_true(validate_column_existence(
    test_data,
    rlang::sym("id"),
    rlang::sym("time"),
    rlang::sym("cause"),
    weights = "weights"
  ))

  # Test with non-existent weights column
  expect_error(
    validate_column_existence(
      test_data,
      rlang::sym("id"),
      rlang::sym("time"),
      rlang::sym("cause"),
      weights = "nonexistent_weights"
    ),
    class = "rlang_error"
  )

  # Test with NULL weights (should work)
  expect_true(validate_column_existence(
    test_data,
    rlang::sym("id"),
    rlang::sym("time"),
    rlang::sym("cause"),
    weights = NULL
  ))
})

# Test updated standardize_data function with weights
test_that("standardize_data works with weights parameter", {
  test_data <- data.frame(
    participant = 1:3,
    follow_time = c(1, 2, 3),
    event_type = c(1, 0, 2),
    start_time = c(0, 0.5, 0),
    wt = c(1.0, 1.5, 0.8)
  )

  # Test with weights and tstart
  result <- standardize_data(
    test_data,
    rlang::sym("participant"),
    rlang::sym("follow_time"),
    rlang::sym("event_type"),
    rlang::sym("start_time"),
    weights = "wt"
  )

  expect_equal(names(result), c("id", "tstart", "time", "cause", "weights"))
  expect_equal(result$weights, c(1.0, 1.5, 0.8))
  expect_equal(result$tstart, c(0, 0.5, 0))

  # Test with weights but no tstart
  result2 <- standardize_data(
    test_data,
    rlang::sym("participant"),
    rlang::sym("follow_time"),
    rlang::sym("event_type"),
    weights = "wt"
  )

  expect_equal(names(result2), c("id", "tstart", "time", "cause", "weights"))
  expect_equal(result2$weights, c(1.0, 1.5, 0.8))
  expect_equal(result2$tstart, c(0, 0, 0))

  # Test without weights
  result3 <- standardize_data(
    test_data,
    rlang::sym("participant"),
    rlang::sym("follow_time"),
    rlang::sym("event_type")
  )

  expect_equal(names(result3), c("id", "tstart", "time", "cause"))
  expect_false("weights" %in% names(result3))
})
