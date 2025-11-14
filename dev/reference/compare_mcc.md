# Compare `mcc` objects

Compares two `mcc` objects and returns a summary of differences. Useful
for comparing results from different methods or parameter settings.

## Usage

``` r
compare_mcc(x, y, tolerance = 1e-06)
```

## Arguments

- x:

  First `mcc` object

- y:

  Second `mcc` object

- tolerance:

  Numeric tolerance for comparing MCC values (default: 1e-6)

## Value

A list summarizing the comparison

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

# Calculate MCC using different methods
mcc_eq <- mcc(df, "id", "time", "cause", method = "equation")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
mcc_sci <- mcc(df, "id", "time", "cause", method = "sci")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Compare the results
comparison <- compare_mcc(mcc_eq, mcc_sci)
print(comparison)
#> 
#> ── MCC Object Comparison ───────────────────────────────────────────────────────
#> ! Objects differ in one or more aspects
#> 
#> ── Comparison Details ──
#> 
#> ✖ Methods match
#> ✔ Weighted status matches
#> ✔ Grouping status matches
#> ✔ Grouping variables match
#> ✔ Final values are close
#> Maximum difference in final values: 0

# Clean up
rm(df, mcc_eq, mcc_sci, comparison)
```
