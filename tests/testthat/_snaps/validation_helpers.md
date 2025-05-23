# validate_column_existence() works

    Code
      validate_column_existence(data = data.frame(a = 1), id_var = "patid", time_var = "survtime",
      cause_var = "status")
    Condition
      Error in `validate_column_existence()`:
      ! Missing required variables in `data`:
      x Missing: patid, survtime, and status

---

    Code
      validate_column_existence(data = data.frame(a = 1), id_var = "patid", time_var = "survtime",
      cause_var = "status", tstart_var = "tstart")
    Condition
      Error in `validate_column_existence()`:
      ! Missing required variables in `data`:
      x Missing: patid, survtime, and status

# validate_data_type() works

    Code
      validate_data_type(data = "not a data.frame")
    Condition
      Error in `validate_data_type()`:
      ! `data` must be a <data.frame> or <tbl_df>

# validate_cause_values() works

    Code
      validate_cause_values(data.frame(x = -1), quote(x))
    Condition
      Error in `validate_cause_values()`:
      ! `cause_var` must only contain values 0, 1, or 2
      x Found invalid values: -1

# validate_time_tstart() works

    Code
      validate_time_tstart(data = data.frame(t = 1:3, tstart = 2:4), time_var = "t",
      tstart_var = "tstart")
    Condition
      Error in `validate_time_tstart()`:
      ! Found 3 cases where event time is not greater than start time.
      i First indices with issues: 1, 2, and 3
      i Ensure all event times are strictly greater than start times.

# standardize_data() works

    Code
      standardize_data(data = mtcars, id_var = 1, time_var = "mpg", cause_var = "cyl",
        tstart_var = "wt")
    Output
                            id tstart time cause
      Mazda RX4           21.0  2.620 21.0     6
      Mazda RX4 Wag       21.0  2.875 21.0     6
      Datsun 710          22.8  2.320 22.8     4
      Hornet 4 Drive      21.4  3.215 21.4     6
      Hornet Sportabout   18.7  3.440 18.7     8
      Valiant             18.1  3.460 18.1     6
      Duster 360          14.3  3.570 14.3     8
      Merc 240D           24.4  3.190 24.4     4
      Merc 230            22.8  3.150 22.8     4
      Merc 280            19.2  3.440 19.2     6
      Merc 280C           17.8  3.440 17.8     6
      Merc 450SE          16.4  4.070 16.4     8
      Merc 450SL          17.3  3.730 17.3     8
      Merc 450SLC         15.2  3.780 15.2     8
      Cadillac Fleetwood  10.4  5.250 10.4     8
      Lincoln Continental 10.4  5.424 10.4     8
      Chrysler Imperial   14.7  5.345 14.7     8
      Fiat 128            32.4  2.200 32.4     4
      Honda Civic         30.4  1.615 30.4     4
      Toyota Corolla      33.9  1.835 33.9     4
      Toyota Corona       21.5  2.465 21.5     4
      Dodge Challenger    15.5  3.520 15.5     8
      AMC Javelin         15.2  3.435 15.2     8
      Camaro Z28          13.3  3.840 13.3     8
      Pontiac Firebird    19.2  3.845 19.2     8
      Fiat X1-9           27.3  1.935 27.3     4
      Porsche 914-2       26.0  2.140 26.0     4
      Lotus Europa        30.4  1.513 30.4     4
      Ford Pantera L      15.8  3.170 15.8     8
      Ferrari Dino        19.7  2.770 19.7     6
      Maserati Bora       15.0  3.570 15.0     8
      Volvo 142E          21.4  2.780 21.4     4

---

    Code
      standardize_data(data = mtcars, id_var = "carb", time_var = NULL, cause_var = "cyl")
    Condition
      Error in `dplyr::relocate()`:
      ! Can't select columns that don't exist.
      x Column `time` doesn't exist.

---

    Code
      standardize_data(data = NULL, time_var = "mpg", id_var = "cyl", cause_var = "disp")
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'select' applied to an object of class "NULL"

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
      Error in `UseMethod()`:
      ! no applicable method for 'group_by' applied to an object of class "c('double', 'numeric')"

---

    Code
      handle_simultaneous_events(data_std = 1, adjust_times = TRUE)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'group_by' applied to an object of class "c('double', 'numeric')"

---

    Code
      handle_simultaneous_events(data_std = 1, adjust_times = TRUE, time_precision = 1)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'group_by' applied to an object of class "c('double', 'numeric')"

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
        id time cause
      1  1    1     1
      2  1    2     2
      
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
      

