# Check if `mcc` object uses weighted estimation

Check if `mcc` object uses weighted estimation

## Usage

``` r
is_weighted(x)
```

## Arguments

- x:

  An `mcc` object

## Value

Logical indicating whether weighted estimation was used

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

# Calculate unweighted MCC
mcc_unweighted <- mcc(df, "id", "time", "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
is_weighted(mcc_unweighted)  # FALSE
#> [1] FALSE

# Create weighted data
df_weighted <- df |>
  group_by(id) |>
  slice(1) |>
  ungroup() |>
  mutate(weights = runif(n(), 0.5, 2.0)) |>
  select(id, weights) |>
  right_join(df, by = "id") |>
  arrange(id, time)

# Calculate weighted MCC
mcc_weighted <- mcc(df_weighted, "id", "time", "cause", weights = "weights")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
is_weighted(mcc_weighted)  # TRUE
#> [1] TRUE

# Clean up
rm(df, df_weighted, mcc_unweighted, mcc_weighted)
```
