# Print method for `mcc` summary objects

Print method for `mcc` summary objects

## Usage

``` r
# S3 method for class 'summary.mcc'
print(x, ...)
```

## Arguments

- x:

  A `summary.mcc` object

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns `x`

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

summary(mcc_eq)
#> 
#> ── Summary of Mean Cumulative Count Results ────────────────────────────────────
#> ℹ Method: Dong-Yasui Equation Method
#> ℹ Total participants: 5
#> 
#> ── Summary Statistics ──
#> 
#> Observation period: [0, 8]
#> Time to MCC = 1.0: 7
#> Time to maximum MCC: 7
#> MCC at end of follow-up: 1
#> 
#> ── Event Count Composition 
#> Events of interest: 4
#> Competing risk events: 2
#> Censoring events: 3

# Calculate MCC using the sum of cumulative incidence method
mcc_sci <- mcc(
  df,
  id_var = "id",
  time_var = "time",
  cause_var = "cause",
  method = "sci"
)
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

print(summary(mcc_sci))
#> 
#> ── Summary of Mean Cumulative Count Results ────────────────────────────────────
#> ℹ Method: Sum of Cumulative Incidence Method
#> ℹ Total participants: 5
#> 
#> ── Summary Statistics ──
#> 
#> Observation period: [0, 8]
#> Time to MCC = 1.0: 7
#> Time to maximum MCC: 7
#> MCC at end of follow-up: 1
#> 
#> ── Event Count Composition 
#> Events of interest: 4
#> Competing risk events: 2
#> Censoring events: 3

# Clean up
rm(df, mcc_eq, mcc_sci)
```
