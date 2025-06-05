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
      1     1 0    
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
      Time range: [1, 7]
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
      # A tibble: 2 x 3
        group  time   mcc
        <chr> <dbl> <dbl>
      1 A         1   0  
      2 A         3   0.5
    Message
      
      -- Group: B 
    Output
      # A tibble: 3 x 3
        group  time   mcc
        <chr> <dbl> <dbl>
      1 B         2   0.5
      2 B         6   1  
      3 B         7   1.5
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
      1 A                2        1        3       0.5
      2 B                3        2        7       1.5

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
      1     1  0   
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

