# Filter `mcc` object by groups

For grouped `mcc` objects, extracts results for specified groups only.
This is useful for focusing on specific groups of interest or creating
custom visualizations.

## Usage

``` r
filter_mcc(x, groups)
```

## Arguments

- x:

  A grouped `mcc` object

- groups:

  Character vector of group names to include

## Value

An `mcc` object containing only the specified groups

## Examples

``` r
# Create sample data with groups
library(dplyr)
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 5, 5, 6, 7, 8),
  time = c(8, 1, 5, 2, 6, 7, 3, 3, 4, 9, 2),
  cause = c(0, 0, 2, 1, 1, 1, 1, 2, 1, 0, 2),
  treatment = c("Control", "Control", "Treatment", "Treatment",
                "Treatment", "Treatment", "Control", "Control",
                "Placebo", "Placebo", "Placebo")
) |>
  arrange(id, time)

# Grouped analysis
mcc_full <- mcc(df, "id", "time", "cause", by = "treatment")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
#> Warning: Found 1 participant where last observation is an event of interest (`cause_var`
#> = 1)
#> ! ID: 4
#> ℹ `mcc()` assumes these participants are censored at their final `time_var`
#> ℹ If participants were actually censored or experienced competing risks after
#>   their last event, add those observations to ensure correct estimates
#> Warning: Found 1 participant where last observation is an event of interest (`cause_var`
#> = 1)
#> ! ID: 6
#> ℹ `mcc()` assumes these participants are censored at their final `time_var`
#> ℹ If participants were actually censored or experienced competing risks after
#>   their last event, add those observations to ensure correct estimates

# Show all groups
mcc_groups(mcc_full)
#> [1] "Control"   "Treatment" "Placebo"  

# Filter to specific groups
mcc_filtered <- filter_mcc(mcc_full, c("Control", "Treatment"))
mcc_groups(mcc_filtered)  # Only "Control" and "Treatment"
#> [1] "Control"   "Treatment"

# Plot the filtered mcc object
plot(mcc_filtered)


# Clean up
rm(df, mcc_full, mcc_filtered)
```
