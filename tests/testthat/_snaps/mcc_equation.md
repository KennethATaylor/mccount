# mcc_equation() handles empty data as expected

    Code
      result <- mcc_equation(data = df, id_var = "id", time_var = "time", cause_var = "cause")
      str(result)
    Output
      List of 3
       $ mcc_final    : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        ..$ time: num(0) 
        ..$ mcc : num(0) 
       $ mcc_table    : tibble [0 x 8] (S3: tbl_df/tbl/data.frame)
        ..$ time                 : num(0) 
        ..$ nrisk                : num(0) 
        ..$ censor               : num(0) 
        ..$ event                : num(0) 
        ..$ cmprk                : num(0) 
        ..$ overall_surv_previous: num(0) 
        ..$ ave_events           : num(0) 
        ..$ mcc                  : num(0) 
       $ original_data:'data.frame':	0 obs. of  4 variables:
        ..$ id    : num(0) 
        ..$ tstart: num(0) 
        ..$ time  : num(0) 
        ..$ cause : num(0) 

# mcc_equation() handles empty data with include_details=FALSE

    Code
      result <- mcc_equation(data = df, id_var = "id", time_var = "time", cause_var = "cause",
        include_details = FALSE)
      str(result)
    Output
      List of 1
       $ mcc_final: tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        ..$ time: num(0) 
        ..$ mcc : num(0) 

