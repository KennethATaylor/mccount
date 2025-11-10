# Extract MCC estimates from `mcc` objects

Extract MCC estimates from `mcc` objects

## Usage

``` r
mcc_estimates(x, ...)
```

## Arguments

- x:

  An `mcc` object

- ...:

  Additional arguments (currently unused)

## Value

A tibble with MCC estimates

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

# Calculate MCC
mcc_result <- mcc(df, "id", "time", "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Extract MCC estimates
estimates <- mcc_estimates(mcc_result)
print(estimates)
#> # A tibble: 5 × 2
#>    time   mcc
#>   <dbl> <dbl>
#> 1     0  0   
#> 2     2  0.25
#> 3     3  0.5 
#> 4     6  0.75
#> 5     7  1   

# For grouped analysis
df_grouped <- df |>
  mutate(group = c("A", "A", "B", "B", "B", "B", "B", "A", "A"))

mcc_grouped <- mcc(df_grouped, "id", "time", "cause", by = "group")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
estimates_grouped <- mcc_estimates(mcc_grouped)
print(estimates_grouped)
#>     group  time   mcc
#>    <char> <num> <num>
#> 1:      A     0   0.0
#> 2:      A     3   0.5
#> 3:      B     0   0.0
#> 4:      B     2   0.5
#> 5:      B     6   1.0
#> 6:      B     7   1.5

# Clean up
rm(df, df_grouped, mcc_result, mcc_grouped, estimates, estimates_grouped)
```
