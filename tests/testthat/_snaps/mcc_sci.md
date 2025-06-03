# mcc_sci() works with snapshot testing for complex cases

    Code
      result <- mcc_sci(data = df, id_var = "id", time_var = "time", cause_var = "cause")
      cat("mcc_final:\n")
    Output
      mcc_final:
    Code
      print(result$mcc_final)
    Output
      # A tibble: 6 x 2
         time SumCIs
        <dbl>  <dbl>
      1     0   0   
      2     5   0.25
      3     6   0.5 
      4     7   0.75
      5    10   1.08
      6    14   1.42
    Code
      cat("\nNumber of CI columns in sci_table:\n")
    Output
      
      Number of CI columns in sci_table:
    Code
      print(sum(grepl("^CI", names(result$sci_table))))
    Output
      [1] 2
    Code
      cat("\nNumber of all_cis elements:\n")
    Output
      
      Number of all_cis elements:
    Code
      print(length(result$all_cis))
    Output
      [1] 2

