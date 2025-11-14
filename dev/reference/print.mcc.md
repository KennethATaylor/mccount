# Print method for `mcc` objects

Print method for `mcc` objects

## Usage

``` r
# S3 method for class 'mcc'
print(x, ...)
```

## Arguments

- x:

  An `mcc` object

- ...:

  Additional arguments (currently unused)

## Value

x invisibly

## Examples

``` r
# Attach dplyr
library(dplyr)
# Create sample data with recurrent events
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
 ) |>
  arrange(id, time)  # Sort the data by id and time

# Calculate MCC using the equation method (default)
mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Print the S3 object (uses print.mcc method)
mcc_eq
#> 
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Dong-Yasui Equation Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time   mcc
#>   <dbl> <dbl>
#> 1     0  0   
#> 2     2  0.25
#> 3     3  0.5 
#> 4     6  0.75
#> 5     7  1   
#> ── Call ──
#> 
#> mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause")

# Calculate MCC using the sum of cumulative incidence method
mcc_sci <- mcc(
  df,
  id_var = "id",
  time_var = "time",
  cause_var = "cause",
  method = "sci"
)
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Print the S3 object
mcc_sci
#> 
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Sum of Cumulative Incidence Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time SumCIs
#>   <dbl>  <dbl>
#> 1     0   0   
#> 2     2   0.25
#> 3     3   0.5 
#> 4     6   0.75
#> 5     7   1   
#> ── Call ──
#> 
#> mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
#>     method = "sci")

# Clean up
rm(df, mcc_eq, mcc_sci)
```
