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
