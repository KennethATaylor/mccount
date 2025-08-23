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
      i Total participants: 4
      
      -- Summary Statistics --
      
      Observation period: "[0, 8]"
      Time to MCC = 1.0: 7
      Time to maximum MCC: 7
      MCC at end of follow-up: 1
      
      -- Event Count Composition 
      Events of interest: 3
      Competing risk events: 1
      Censoring events: 3

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
      i Total participants: 5
      i Overall observation period: "[0, 8]"
      
      -- Summary by Group (group) --
      
      -- Group: "A" 
      Participants in group: 3
      Group observation period: "[0, 8]"
      Time to MCC = 1.0: "Never reached"
      Time to maximum MCC: 3
      MCC at end of follow-up: 0.5
      Events of interest: 1
      Competing risk events: 1
      Censoring events: 2
      
      
      -- Group: "B" 
      Participants in group: 2
      Group observation period: "[0, 8]"
      Time to MCC = 1.0: 6
      Time to maximum MCC: 7
      MCC at end of follow-up: 1.5
      Events of interest: 3
      Competing risk events: 1
      Censoring events: 1

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
      i Total participants: 1
      
      -- Summary Statistics --
      
      Observation period: "[0, 1]"
      Time to MCC = 1.0: "Never reached"
      Time to maximum MCC: 0
      MCC at end of follow-up: 0
      
      -- Event Count Composition 
      Events of interest: 0
      Competing risk events: 0
      Censoring events: 1

