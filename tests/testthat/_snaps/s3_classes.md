# Print methods work without errors

    Code
      print(mcc_result)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- MCC Estimates --
      
    Output
      # A tibble: 4 x 2
         time   mcc
        <dbl> <dbl>
      1     0 0    
      2     2 0.333
      3     6 0.667
      4     7 1    
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause")

---

    Code
      print(summary_obj)
    Message
      
      -- Summary of Mean Cumulative Count Results ------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- Summary Statistics --
      
      Number of time points: 4
      Time range: [0, 7]
      Final MCC: 1

# Print methods work for grouped objects

    Code
      print(mcc_grouped)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      i Grouped by: group (2 groups)
      
      -- MCC Estimates --
      
      -- Group: A 
    Output
          group  time   mcc
         <char> <num> <num>
      1:      A     0   0.0
      2:      A     3   0.5
    Message
      
      -- Group: B 
    Output
          group  time   mcc
         <char> <num> <num>
      1:      B     0   0.0
      2:      B     2   0.5
      3:      B     6   1.0
      4:      B     7   1.5
    Message
      
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          by = "group")

---

    Code
      print(summary(mcc_grouped))
    Message
      
      -- Summary of Mean Cumulative Count Results ------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- Summary by Group (group) --
      
    Output
      # A tibble: 2 x 5
        group n_timepoints min_time max_time final_mcc
        <chr>        <int>    <dbl>    <dbl>     <dbl>
      1 A                2        0        3       0.5
      2 B                4        0        7       1.5

# Print methods work for weighted objects

    Code
      print(mcc_weighted)
    Message
      
      -- Mean Cumulative Count Results -----------------------------------------------
      i Method: Dong-Yasui Equation Method
      i Weighted estimation: Yes
      
      -- MCC Estimates --
      
    Output
      # A tibble: 5 x 2
         time   mcc
        <dbl> <dbl>
      1     0  0   
      2     2  0.24
      3     3  0.64
      4     6  0.88
      5     7  1.12
    Message
      -- Call --
      
      mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
          weights = "weights")

# MCC comparison print works correctly

    Code
      print(comp1)
    Message
      
      -- MCC Object Comparison -------------------------------------------------------
      v Objects are equivalent within tolerance
      
      -- Comparison Details --
      
      v Methods match
      v Weighted status matches
      v Grouping status matches
      v Grouping variables match
      v Final values are close
      Maximum difference in final values: 0

---

    Code
      print(comp2)
    Message
      
      -- MCC Object Comparison -------------------------------------------------------
      ! Objects differ in one or more aspects
      
      -- Comparison Details --
      
      x Methods match
      v Weighted status matches
      v Grouping status matches
      v Grouping variables match
      v Final values are close
      Maximum difference in final values: 0

# compare_mcc works correctly

    Code
      print(comp_same)
    Message
      
      -- MCC Object Comparison -------------------------------------------------------
      v Objects are equivalent within tolerance
      
      -- Comparison Details --
      
      v Methods match
      v Weighted status matches
      v Grouping status matches
      v Grouping variables match
      v Final values are close
      Maximum difference in final values: 0

---

    Code
      print(comp_diff)
    Message
      
      -- MCC Object Comparison -------------------------------------------------------
      ! Objects differ in one or more aspects
      
      -- Comparison Details --
      
      x Methods match
      v Weighted status matches
      v Grouping status matches
      v Grouping variables match
      v Final values are close
      Maximum difference in final values: 0

# S3 methods handle edge cases correctly

    Code
      summary(mcc_minimal)
    Message
      
      -- Summary of Mean Cumulative Count Results ------------------------------------
      i Method: Dong-Yasui Equation Method
      
      -- Summary Statistics --
      
      Number of time points: 1
      Time range: [0, 0]
      Final MCC: 0

