# Check if object is an `mcc` result

Check if object is an `mcc` result

## Usage

``` r
is_mcc(x)
```

## Arguments

- x:

  An object to test

## Value

TRUE if x is an `mcc` object, FALSE otherwise

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

# Test if it's an MCC object
is_mcc(mcc_result)  # TRUE
#> [1] TRUE

# Clean up
rm(df, mcc_result)
```
