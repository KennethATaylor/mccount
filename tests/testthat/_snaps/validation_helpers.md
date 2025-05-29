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

# validate_data_type() works with various data types

    Code
      validate_data_type(list(x = 1))
    Condition
      Error in `validate_data_type()`:
      ! `data` must be a <data.frame> or <tbl_df>

---

    Code
      validate_data_type(matrix(1:4, nrow = 2))
    Condition
      Error in `validate_data_type()`:
      ! `data` must be a <data.frame> or <tbl_df>

---

    Code
      validate_data_type(c(1, 2, 3))
    Condition
      Error in `validate_data_type()`:
      ! `data` must be a <data.frame> or <tbl_df>

---

    Code
      validate_data_type(NULL)
    Condition
      Error in `validate_data_type()`:
      ! `data` must be a <data.frame> or <tbl_df>

# validate_cause_values() works with valid values

    Code
      validate_cause_values(data.frame(status = c(-1, 3, 5)), quote(status))
    Condition
      Error in `validate_cause_values()`:
      ! `cause_var` must only contain values 0, 1, or 2
      x Found invalid values: -1, 3, and 5

---

    Code
      validate_cause_values(data.frame(outcome = c(0, 1, 3, 2, -1)), quote(outcome))
    Condition
      Error in `validate_cause_values()`:
      ! `cause_var` must only contain values 0, 1, or 2
      x Found invalid values: 3 and -1

# validate_time_tstart() works with valid values

    Code
      validate_time_tstart(data = data.frame(time = c(5, 5), tstart = c(5, 3)),
      time_var = "time", tstart_var = "tstart")
    Condition
      Error in `validate_time_tstart()`:
      ! Found 1 case where event time is not greater than start time.
      i First indices with issues: 1
      i Ensure all event times are strictly greater than start times.

# validate_by_variable() works

    Code
      validate_by_variable(data.frame(x = 1), 123)
    Condition
      Error in `validate_by_variable()`:
      ! `by` must be a single character string
      x Received: 123

---

    Code
      validate_by_variable(data.frame(x = 1), c("group1", "group2"))
    Condition
      Error in `validate_by_variable()`:
      ! `by` must be a single character string
      x Received: "group1" and "group2"

---

    Code
      validate_by_variable(data.frame(x = 1), "missing_col")
    Condition
      Error in `validate_by_variable()`:
      ! Column specified in `by` not found in `data`
      x Column 'missing_col' does not exist

---

    Code
      validate_by_variable(data.frame(group = c(NA, NA, NA)), "group")
    Condition
      Error in `validate_by_variable()`:
      ! All values in `by` variable are missing (NA)
      x Column 'group' contains only NA values

---

    Code
      validate_by_variable(many_groups_data, "group")
    Condition
      Warning:
      Large number of groups detected in `by` variable
      i Found 25 unique groups in 'group'
      i Consider whether this many groups is intended
    Output
      [1] TRUE

