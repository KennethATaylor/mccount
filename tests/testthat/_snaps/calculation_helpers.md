# handle_simultaneous_events() works

    Code
      handle_simultaneous_events()
    Condition
      Error in `handle_simultaneous_events()`:
      ! argument "data_std" is missing, with no default

---

    Code
      handle_simultaneous_events(data_std = 1)
    Condition
      Error in `setorderv()`:
      ! some columns are not in the data.table: [id, time]

---

    Code
      handle_simultaneous_events(data_std = 1, adjust_times = TRUE)
    Condition
      Error in `setorderv()`:
      ! some columns are not in the data.table: [id, time]

---

    Code
      handle_simultaneous_events(data_std = 1, adjust_times = TRUE, time_precision = 1)
    Condition
      Error in `setorderv()`:
      ! some columns are not in the data.table: [id, time]

---

    Code
      handle_simultaneous_events(data_std = data.frame(id = 1, time = 1),
      adjust_times = TRUE, time_precision = 1)
    Output
      $data
        id time
      1  1    1
      
      $times_were_adjusted
      [1] FALSE
      

---

    Code
      handle_simultaneous_events(data_std = data.frame(id = 1, time = c(1, 1), cause = c(
        1, 2)), adjust_times = TRUE, time_precision = 1)
    Message
      i Adjusted time points for events occurring simultaneously for the same subject.
    Output
      $data
      # A tibble: 2 x 3
           id  time cause
        <dbl> <dbl> <dbl>
      1     1     1     1
      2     1     2     2
      
      $times_were_adjusted
      [1] TRUE
      

---

    Code
      handle_simultaneous_events(data_std = data.frame(id = 1, time = c(1, 1), cause = c(
        1, 2)), adjust_times = FALSE, time_precision = 1)
    Condition
      Warning:
      Data contains events occurring simultaneously for the same subject.
      i These events will be processed without time adjustment (`adjust_times = FALSE`).
      i This may affect calculation accuracy. Consider using `adjust_times = TRUE`.
    Output
      $data
        id time cause
      1  1    1     1
      2  1    1     2
      
      $times_were_adjusted
      [1] FALSE
      

