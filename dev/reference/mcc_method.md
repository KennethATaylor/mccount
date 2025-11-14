# Get the method used for MCC calculation

Get the method used for MCC calculation

## Usage

``` r
mcc_method(x)
```

## Arguments

- x:

  An `mcc` object

## Value

Character string indicating the method ("equation" or "sci")

## Examples

``` r
# Create sample data
library(dplyr)
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
  group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
) |>
  arrange(id, time)

# Calculate MCC
mcc_result <- mcc(df, "id", "time", "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Get the method used
mcc_method(mcc_result)
#> [1] "equation"

# Clean up
rm(df, mcc_result)
```
