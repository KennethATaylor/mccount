# mcc() validates the method argument correctly

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "equation")
    Output
      $mcc_final
      # A tibble: 1 x 2
         time   mcc
        <dbl> <dbl>
      1     5     1
      
      $mcc_table
      # A tibble: 1 x 8
         time nrisk censor event cmprk overall_surv_previous ave_events   mcc
        <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
      1     5     1      1     1     0                     1          1     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "equation"
      

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", method = "sci")
    Output
      $mcc_final
      # A tibble: 2 x 2
         time SumCIs
        <dbl>  <dbl>
      1     0      0
      2     5      1
      
      $sci_table
      # A tibble: 1 x 3
         time   CI1 SumCIs
        <dbl> <dbl>  <dbl>
      1     5     1      1
      
      $all_cis
      $all_cis[[1]]
      # A tibble: 4 x 2
         time    ci
        <dbl> <dbl>
      1     0     0
      2     5     0
      3     5     1
      4     5     1
      
      
      $mcc_base
      # A tibble: 4 x 4
         time    cm  Deta  cumI
        <dbl> <dbl> <dbl> <int>
      1     0     0     0     1
      2     5     0     0     1
      3     5     1     1     1
      4     5     1     0     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "sci"
      

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
    Output
      $mcc_final
      # A tibble: 1 x 2
         time   mcc
        <dbl> <dbl>
      1     5     1
      
      $mcc_table
      # A tibble: 1 x 8
         time nrisk censor event cmprk overall_surv_previous ave_events   mcc
        <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
      1     5     1      1     1     0                     1          1     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "equation"
      

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", time_precision = 1e-10)
    Output
      $mcc_final
      # A tibble: 1 x 2
         time   mcc
        <dbl> <dbl>
      1     5     1
      
      $mcc_table
      # A tibble: 1 x 8
         time nrisk censor event cmprk overall_surv_previous ave_events   mcc
        <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
      1     5     1      1     1     0                     1          1     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "equation"
      

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
    Output
      $mcc_final
      # A tibble: 1 x 2
         time SumCIs
        <dbl>  <dbl>
      1     5    0.5
      
      $sci_table
      # A tibble: 2 x 3
         time   CI1 SumCIs
        <dbl> <dbl>  <dbl>
      1     5   0.5    0.5
      2     8   0.5    0.5
      
      $all_cis
      $all_cis[[1]]
      # A tibble: 1 x 2
         time    ci
        <dbl> <dbl>
      1     5   0.5
      
      
      $mcc_base
      # A tibble: 1 x 4
         time    cm  Deta  cumI
        <dbl> <dbl> <dbl> <int>
      1     5   0.5   0.5     1
      
      $original_data
        id tstart time cause
      1  1      1    5     1
      2  2      2    8     0
      
      $method
      [1] "sci"
      

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
    Output
      $mcc_final
      # A tibble: 1 x 2
         time   mcc
        <dbl> <dbl>
      1     5     1
      
      $mcc_table
      # A tibble: 1 x 8
         time nrisk censor event cmprk overall_surv_previous ave_events   mcc
        <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
      1     5     1      1     1     0                     1          1     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "equation"
      

---

    Code
      mcc(df, id_var = "id", time_var = "time", cause_var = "cause", adjust_times = FALSE)
    Output
      $mcc_final
      # A tibble: 1 x 2
         time   mcc
        <dbl> <dbl>
      1     5     1
      
      $mcc_table
      # A tibble: 1 x 8
         time nrisk censor event cmprk overall_surv_previous ave_events   mcc
        <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
      1     5     1      1     1     0                     1          1     1
      
      $original_data
        id tstart time cause
      1  1      0    5     1
      
      $method
      [1] "equation"
      

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
      # A tibble: 4 x 2
         time   mcc
        <dbl> <dbl>
      1     5  0.25
      2     6  0.5 
      3     8  0.75
      4    15  1.25

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

