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
