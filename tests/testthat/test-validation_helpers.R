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


test_that("handle_simultaneous_events() works", {
  expect_snapshot(error = TRUE, handle_simultaneous_events())
  expect_snapshot(error = TRUE, handle_simultaneous_events(data_std = 1))
  expect_snapshot(
    error = TRUE,
    handle_simultaneous_events(data_std = 1, adjust_times = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    handle_simultaneous_events(
      data_std = 1,
      adjust_times = TRUE,
      time_precision = 1
    )
  )
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = 1),
    adjust_times = TRUE,
    time_precision = 1
  ))
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = c(1, 1), cause = c(1, 2)),
    adjust_times = TRUE,
    time_precision = 1
  ))
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = c(1, 1), cause = c(1, 2)),
    adjust_times = FALSE,
    time_precision = 1
  ))
})
