# Get grouping variable name from grouped `mcc` object

Get grouping variable name from grouped `mcc` object

## Usage

``` r
mcc_grouping_var(x)
```

## Arguments

- x:

  An `mcc` object

## Value

Character string with grouping variable name, or NULL if not grouped

## Examples

``` r
# Create sample data with groups
library(dplyr)
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
  treatment = c("Control", "Control", "Treatment", "Treatment",
                "Treatment", "Treatment", "Treatment", "Control",
                "Control")
) |>
  arrange(id, time)

# Grouped analysis
mcc_grouped <- mcc(df, "id", "time", "cause", by = "treatment")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Get grouping variable name
mcc_grouping_var(mcc_grouped)  # "treatment"
#> [1] "treatment"

# Clean up
rm(df, mcc_grouped)
```
