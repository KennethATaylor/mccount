# Check if `mcc` object is from grouped analysis

Check if `mcc` object is from grouped analysis

## Usage

``` r
is_grouped(x)
```

## Arguments

- x:

  An `mcc` object

## Value

Logical indicating whether the analysis was grouped

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

# Ungrouped analysis
mcc_ungrouped <- mcc(df, "id", "time", "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
is_grouped(mcc_ungrouped)  # FALSE
#> [1] FALSE

# Grouped analysis
mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
is_grouped(mcc_grouped)  # TRUE
#> [1] TRUE

# Clean up
rm(df, mcc_ungrouped, mcc_grouped)
```
