# mcc() validates the method argument correctly

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "equation")
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time   mcc
        <dbl> <dbl>
      1     0     0
      2     5     1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          method = "equation")

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "sci")
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Sum of Cumulative Incidence Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time SumCIs
        <dbl>  <dbl>
      1     0      0
      2     5      1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          method = "sci")

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "equation", "sci"

# mcc() correctly validates time_precision

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = "not_numeric")
    Condition
      Error in `mcc()`:
      ! `time_precision` must be a positive numeric value
      x Received: "not_numeric"

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = -
        1)
    Condition
      Error in `mcc()`:
      ! `time_precision` must be a positive numeric value
      x Received: -1

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = c(
        1, 2))
    Condition
      Error in `mcc()`:
      ! `time_precision` must be a positive numeric value
      x Received: 1 and 2

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = 0.1)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time   mcc
        <dbl> <dbl>
      1     0     0
      2     5     1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          time_precision = 0.1)

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = 1e-10)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time   mcc
        <dbl> <dbl>
      1     0     0
      2     5     1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          time_precision = 1e-10)

# mcc() validates tstart_var compatibility with method

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "equation",
        tstart_var = "tstart")
    Condition
      Error in `mcc()`:
      ! `tstart_var` is only compatible with `method = "sci"`
      i You specified `method = "equation"`, which does not support start times
      i Either change to `method = "sci"` or remove the `tstart_var` parameter

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "sci",
        tstart_var = "tstart")
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Sum of Cumulative Incidence Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time SumCIs
        <dbl>  <dbl>
      1     0    0  
      2     5    0.5
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          method = "sci", tstart_var = "tstart")

# mcc() validates adjust_times correctly

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = "TRUE")
    Condition
      Error in `mcc()`:
      ! `adjust_times` must be a <logical> value (`TRUE` or `FALSE`)
      x Received: "TRUE"

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = 1)
    Condition
      Error in `mcc()`:
      ! `adjust_times` must be a <logical> value (`TRUE` or `FALSE`)
      x Received: 1

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = c(
        TRUE, FALSE))
    Condition
      Error in `mcc()`:
      ! `adjust_times` must be a <logical> value (`TRUE` or `FALSE`)
      x Received: TRUE and FALSE

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = TRUE)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time   mcc
        <dbl> <dbl>
      1     0     0
      2     5     1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          adjust_times = TRUE)

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = FALSE)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 2 x 2
         time   mcc
        <dbl> <dbl>
      1     0     0
      2     5     1
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          adjust_times = FALSE)

# mcc() end-to-end functionality through snapshots

    Code
      result_eq <- mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause",
        method = "equation")
      cat("MCC with equation method:\n")
    Output
      MCC with equation method:
    Code
      print(result_eq$mcc_final)
    Output
      # A tibble: 5 x 2
         time   mcc
        <dbl> <dbl>
      1     0  0   
      2     5  0.25
      3     6  0.5 
      4     8  0.75
      5    15  1.25

---

    Code
      result_sci <- mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause",
        method = "sci")
      cat("MCC with sci method:\n")
    Output
      MCC with sci method:
    Code
      print(result_sci$mcc_final)
    Output
      # A tibble: 5 x 2
         time SumCIs
        <dbl>  <dbl>
      1     0   0   
      2     5   0.25
      3     6   0.5 
      4     8   0.75
      5    15   1.25

# mcc() validates include_details parameter

    Code
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause",
        include_details = "TRUE")
    Condition
      Error in `mcc()`:
      ! `include_details` must be a <logical> value (`TRUE` or `FALSE`)
      x Received: "TRUE"

---

    Code
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause",
        include_details = c(TRUE, FALSE))
    Condition
      Error in `mcc()`:
      ! `include_details` must be a <logical> value (`TRUE` or `FALSE`)
      x Received: TRUE and FALSE

# mcc() validates by argument correctly

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", by = 123)
    Condition
      Error in `validate_by_variable()`:
      ! `by` must be a single character string
      x Received: 123

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", by = c("group1",
        "group2"))
    Condition
      Error in `validate_by_variable()`:
      ! `by` must be a single character string
      x Received: "group1" and "group2"

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", by = "missing_column")
    Condition
      Error in `validate_by_variable()`:
      ! Column specified in `by` not found in `data`
      x Column 'missing_column' does not exist

---

    Code
      mcc(df_na, id_var = "id", time_var = "time", cause_var = "cause", by = "group")
    Condition
      Error in `validate_by_variable()`:
      ! All values in `by` variable are missing (NA)
      x Column 'group' contains only NA values

# mcc_by_group() handles all empty groups scenario

    Code
      mcc(df_all_na, id_var = "id", time_var = "time", cause_var = "cause", by = "group")
    Condition
      Error in `validate_by_variable()`:
      ! All values in `by` variable are missing (NA)
      x Column 'group' contains only NA values

# mcc() with by argument warning for many groups

    Code
      result <- mcc(many_groups_df, id_var = "id", time_var = "time", cause_var = "cause",
        by = "group")
    Condition
      Warning:
      Large number of groups detected in `by` variable
      i Found 25 unique groups in 'group'
      i Consider whether this many groups is intended

