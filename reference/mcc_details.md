# Extract calculation details from `mcc` objects

Extract calculation details from `mcc` objects

## Usage

``` r
mcc_details(x, ...)
```

## Arguments

- x:

  An `mcc` object

- ...:

  Additional arguments (currently unused)

## Value

A tibble with calculation details, or NULL if not available

## Examples

``` r
# Create sample data
library(dplyr)
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
 ) |>
  arrange(id, time)

# Calculate MCC with details
mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Extract calculation details
details_eq <- mcc_details(mcc_eq)   # Returns mcc_table
details_sci <- mcc_details(mcc_sci) # Returns sci_table

print(details_eq)
#> # A tibble: 9 × 8
#>    time nrisk censor event cmprk overall_surv_previous ave_events   mcc
#>   <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
#> 1  0        5      0     0     0                  1          0     0   
#> 2  1        5      1     0     0                  1          0     0   
#> 3  2        4      0     1     0                  1          0.25  0.25
#> 4  3        4      0     1     0                  1          0.25  0.5 
#> 5  3.00     4      0     0     1                  1          0     0.5 
#> 6  5        3      0     0     1                  0.75       0     0.5 
#> 7  6        2      0     1     0                  0.5        0.25  0.75
#> 8  7        2      0     1     0                  0.5        0.25  1   
#> 9  8        2      2     0     0                  0.5        0     1   
print(details_sci)
#> # A tibble: 9 × 5
#>    time   CI1   CI2   CI3 SumCIs
#>   <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1  0     0     0     0      0   
#> 2  1     0     0     0      0   
#> 3  2     0.25  0     0      0.25
#> 4  3     0.5   0     0      0.5 
#> 5  3.00  0.5   0     0      0.5 
#> 6  5     0.5   0     0      0.5 
#> 7  6     0.5   0.25  0      0.75
#> 8  7     0.5   0.25  0.25   1   
#> 9  8     0.5   0.25  0.25   1   

# Clean up
rm(df, mcc_eq, mcc_sci, details_eq, details_sci)
```
