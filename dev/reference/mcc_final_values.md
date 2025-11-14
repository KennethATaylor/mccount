# Get final MCC value for each group

Extracts the final (maximum time) MCC value for each group in a grouped
analysis, or the overall final MCC value for ungrouped analyses.

## Usage

``` r
mcc_final_values(x)
```

## Arguments

- x:

  An `mcc` object

## Value

A named numeric vector with final MCC values

## Examples

``` r
# Create sample data
library(dplyr)
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 1, 2),
  group = c("A", "A", "B", "B", "B", "B", "A", "A")
) |>
  arrange(id, time)

# Ungrouped analysis
mcc_ungrouped <- mcc(df, "id", "time", "cause")
#> Warning: Found 1 participant where last observation is an event of interest (`cause_var`
#> = 1)
#> ! ID: 4
#> ℹ `mcc()` assumes these participants are censored at their final `time_var`
#> ℹ If participants were actually censored or experienced competing risks after
#>   their last event, add those observations to ensure correct estimates
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
mcc_final_values(mcc_ungrouped)
#> Overall 
#>       1 

# Grouped analysis
mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
#> Warning: Found 1 participant where last observation is an event of interest (`cause_var`
#> = 1)
#> ! ID: 4
#> ℹ `mcc()` assumes these participants are censored at their final `time_var`
#> ℹ If participants were actually censored or experienced competing risks after
#>   their last event, add those observations to ensure correct estimates
mcc_final_values(mcc_grouped)
#>   A   B 
#> 0.5 1.5 

# Clean up
rm(df, mcc_ungrouped, mcc_grouped)
```
