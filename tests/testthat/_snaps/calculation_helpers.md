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
      

# add_group_column_to_result() output format snapshot

    Code
      result <- add_group_column_to_result(mock_result, "treatment_group", "Active")
      cat("Structure after adding group column:\n")
    Output
      Structure after adding group column:
    Code
      print(str(result, max.level = 2))
    Output
      List of 4
       $ mcc_final    : tibble [3 x 3] (S3: tbl_df/tbl/data.frame)
       $ mcc_table    : tibble [3 x 9] (S3: tbl_df/tbl/data.frame)
       $ original_data: tibble [3 x 5] (S3: tbl_df/tbl/data.frame)
       $ method       : chr "equation"
      NULL
    Code
      cat("\nMCC Final:\n")
    Output
      
      MCC Final:
    Code
      print(result$mcc_final)
    Output
      # A tibble: 3 x 3
        treatment_group  time   mcc
        <chr>           <dbl> <dbl>
      1 Active              5   0.1
      2 Active             10   0.3
      3 Active             15   0.5
    Code
      cat("\nMCC Table (first 5 columns):\n")
    Output
      
      MCC Table (first 5 columns):
    Code
      print(result$mcc_table[, 1:5])
    Output
      # A tibble: 3 x 5
        treatment_group  time nrisk event censor
        <chr>           <dbl> <dbl> <dbl>  <dbl>
      1 Active              5   100    10      5
      2 Active             10    80    15     10
      3 Active             15    60    20     15
    Code
      cat("\nOriginal Data:\n")
    Output
      
      Original Data:
    Code
      print(result$original_data)
    Output
      # A tibble: 3 x 5
        treatment_group    id tstart  time cause
        <chr>           <dbl>  <dbl> <dbl> <dbl>
      1 Active              1      0     5     1
      2 Active              2      0    10     1
      3 Active              3      0    15     1

# combine_group_results() output format snapshot

    Code
      combined_detailed <- combine_group_results(group_results, "treatment",
        include_details = TRUE)
      cat("Combined results with include_details = TRUE:\n")
    Output
      Combined results with include_details = TRUE:
    Code
      cat("Components:", names(combined_detailed), "\n\n")
    Output
      Components: mcc_final mcc_table original_data 
      
    Code
      cat("MCC Final:\n")
    Output
      MCC Final:
    Code
      print(combined_detailed$mcc_final)
    Output
      # A tibble: 5 x 3
        treatment  time   mcc
      * <chr>     <dbl> <dbl>
      1 Control       5  0.1 
      2 Control      10  0.25
      3 Control      15  0.4 
      4 Treatment     8  0.15
      5 Treatment    12  0.35
    Code
      cat("\nMCC Table:\n")
    Output
      
      MCC Table:
    Code
      print(combined_detailed$mcc_table)
    Output
      # A tibble: 5 x 5
        treatment  time nrisk event   mcc
      * <chr>     <dbl> <dbl> <dbl> <dbl>
      1 Control       5    50     5  0.1 
      2 Control      10    40     8  0.25
      3 Control      15    30    10  0.4 
      4 Treatment     8    45     7  0.15
      5 Treatment    12    35     9  0.35
    Code
      cat("\nOriginal Data:\n")
    Output
      
      Original Data:
    Code
      print(combined_detailed$original_data)
    Output
      # A tibble: 5 x 5
        treatment    id tstart  time cause
      * <chr>     <dbl>  <dbl> <dbl> <dbl>
      1 Control       1      0     5     1
      2 Control       2      0    10     1
      3 Control       3      0    15     1
      4 Treatment     4      0     8     1
      5 Treatment     5      0    12     1

---

    Code
      combined_simple <- combine_group_results(group_results, "treatment",
        include_details = FALSE)
      cat("Combined results with include_details = FALSE:\n")
    Output
      Combined results with include_details = FALSE:
    Code
      cat("Components:", names(combined_simple), "\n\n")
    Output
      Components: mcc_final 
      
    Code
      cat("MCC Final:\n")
    Output
      MCC Final:
    Code
      print(combined_simple$mcc_final)
    Output
      # A tibble: 5 x 3
        treatment  time   mcc
      * <chr>     <dbl> <dbl>
      1 Control       5  0.1 
      2 Control      10  0.25
      3 Control      15  0.4 
      4 Treatment     8  0.15
      5 Treatment    12  0.35

# combine_group_results() with SCI method snapshot

    Code
      cat("SCI Combined results:\n")
    Output
      SCI Combined results:
    Code
      cat("Components:", names(combined_sci), "\n\n")
    Output
      Components: mcc_final sci_table mcc_base all_cis 
      
    Code
      cat("MCC Final:\n")
    Output
      MCC Final:
    Code
      print(combined_sci$mcc_final)
    Output
      # A tibble: 3 x 3
        group  time SumCIs
      * <chr> <dbl>  <dbl>
      1 A         5   0.1 
      2 A        10   0.3 
      3 B         8   0.15
    Code
      cat("\nSCI Table:\n")
    Output
      
      SCI Table:
    Code
      print(combined_sci$sci_table)
    Output
      # A tibble: 3 x 5
        group  time   CI1   CI2 SumCIs
      * <chr> <dbl> <dbl> <dbl>  <dbl>
      1 A         5  0.08  0.02   0.1 
      2 A        10  0.2   0.1    0.3 
      3 B         8  0.15  0      0.15
    Code
      cat("\nMCC Base:\n")
    Output
      
      MCC Base:
    Code
      print(combined_sci$mcc_base)
    Output
      # A tibble: 3 x 5
        group  time    cm  Deta  cumI
      * <chr> <dbl> <dbl> <dbl> <dbl>
      1 A         5  0.08  0.08     1
      2 A        10  0.2   0.12     1
      3 B         8  0.15  0.15     1
    Code
      cat("\nAll CIs structure:\n")
    Output
      
      All CIs structure:
    Code
      cat("Groups in all_cis:", names(combined_sci$all_cis), "\n")
    Output
      Groups in all_cis: A B 
    Code
      cat("Number of CI lists per group:\n")
    Output
      Number of CI lists per group:
    Code
      for (group_name in names(combined_sci$all_cis)) {
        cat(paste0("  ", group_name, ": ", length(combined_sci$all_cis[[group_name]]),
        " CI tables\n"))
      }
    Output
        A: 2 CI tables
        B: 1 CI tables
    Code
      cat("\nFirst CI table for group A:\n")
    Output
      
      First CI table for group A:
    Code
      print(combined_sci$all_cis$A[[1]])
    Output
      # A tibble: 2 x 3
        group  time    ci
        <chr> <dbl> <dbl>
      1 A         5  0.08
      2 A        10  0.2 

