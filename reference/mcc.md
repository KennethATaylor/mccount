# Calculate Mean Cumulative Count (MCC)

Calculates the mean cumulative count (MCC), which estimates the expected
cumulative number of events per person over time, while accounting for
potential competing risks and censoring. This function provides a
unified interface to two different estimation approaches: the Dong-Yasui
(`"equation"`) method and the sum of cumulative incidence (`"sci"`)
method.

The `"equation"` method calculates MCC directly through probability
calculations, while the `"sci"` method derives MCC by summing the
cumulative incidence functions for each recurrent event. The two
approaches yield equivalent results in certain circumstances. When they
do not, the choice between methods depends on the specific outcome,
analysis needs, and data structure. See
[`vignette("choosing-between-methods")`](https://kennethataylor.github.io/mccount/articles/choosing-between-methods.md)
for more details.

## Usage

``` r
mcc(
  data,
  id_var,
  time_var,
  cause_var,
  by = NULL,
  method = c("equation", "sci"),
  tstart_var = NULL,
  weights = NULL,
  adjust_times = TRUE,
  time_precision = 1e-06,
  include_details = TRUE
)
```

## Arguments

- data:

  (`data.frame` or `tbl_df`)  
  A `data.frame` or tibble containing the required variables

- id_var:

  (`string`)  
  Name of the column containing participant identifiers

- time_var:

  (`string`)  
  Name of the column containing follow-up times

- cause_var:

  (`string`)  
  Name of the column containing event indicator values (1 = event of
  interest, 2 = competing risk, 0 = censoring)

- by:

  (`string`, optional)  
  Name of the column to group by for calculating MCC within subgroups.
  If provided, MCC will be calculated separately for each level of this
  variable

- method:

  (`string`)  
  Method to use for MCC calculation. Either `"equation"` (default) or
  `"sci"` (sum of cumulative incidence)

- tstart_var:

  (`string`)  
  Name of the column containing start times of follow-up for
  incorporating left truncation. Only allowed to be specified when
  `method = "sci"`. If `NULL` (default), a constant value of `0` is used
  in calculation (i.e., right truncation only)

- weights:

  (`string`, optional)  
  Name of the column containing weights for weighted MCC estimation.
  Currently only supported with `method = "equation"`. If provided, all
  weights must be non-negative and non-missing

- adjust_times:

  (`logical`)  
  If `TRUE` (default), automatically adjusts times to account for
  outcome events and competing risk events occurring at the same time

- time_precision:

  (`numeric`)  
  Precision used for adjusting simultaneous events (default: 1e-6). Must
  be a positive numeric value

- include_details:

  (`logical`)  
  Whether to include detailed calculation tables and intermediate
  objects in the output. Default is `TRUE`, which returns all
  calculation details. Setting to `FALSE` returns only the final MCC
  estimates, making the function more efficient for bootstrapping

## Value

An S3 object of class `"mcc"` with method-specific subclasses. The
object contains:

When `include_details = TRUE` (default):

For `method = "equation"`:

- `mcc_final`: A tibble with columns for `time` and `mcc`

- `mcc_table`: A tibble with detailed calculation steps

- `original_data`: The input `data` with standardized column names

- `adjusted_data`: Present only if time adjustments were applied

For `method = "sci"`:

- `mcc_final`: A tibble with columns for `time` and MCC (expressed as
  `SumCIs`)

- `sci_table`: A tibble with cumulative incidence for each event number
  and their sum

- `all_cis`: A list of cumulative incidence data for each event number

- `mcc_base`: A tibble with calculation details for the MCC

- `original_data`: The input `data` with standardized column names

- `adjusted_data`: Present only if time adjustments were applied

When `include_details = FALSE`:

- `mcc_final`: A tibble with columns for `time` and `mcc` (or `SumCIs`
  for `method = "sci"`)

All objects include metadata:

- `method`: The method used for calculation

- `weighted`: Logical indicating whether weighted estimation was used

- `by_group`: Name of grouping variable (for grouped analyses)

- `call`: The original function call

When `by` is specified, all tibbles contain an additional column with
the grouping variable values, and the object has the additional class
`"mcc_grouped"`.

## References

Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
Estimating the burden of recurrent events in the presence of competing
risks: the method of mean cumulative count. *Am J Epidemiol*. 2015 Apr
1;181(7):532-40. doi:
[10.1093/aje/kwu289](https://www.doi.org/10.1093/aje/kwad031)

Gaber CE, Edwards JK, Lund JL, Peery AF, Richardson DB, Kinlaw AC.
Inverse Probability Weighting to Estimate Exposure Effects on the Burden
of Recurrent Outcomes in the Presence of Competing Events. *Am J
Epidemiol*. 2023;192(5):830-839. doi:
[10.1093/aje/kwad031](https://www.doi.org/10.1093/aje/kwad031)

## Examples

``` r
# Attach dplyr
library(dplyr)
# Create sample data with recurrent events
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3), # Times will be adjusted for id = 5
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
 ) |>
  arrange(id, time)  # Sort the data by id and time

# Print the dataset
print("Hypothetical dataset from Dong et al. (2015):")
#> [1] "Hypothetical dataset from Dong et al. (2015):"
print(df)
#>   id time cause
#> 1  1    8     0
#> 2  2    1     0
#> 3  3    5     2
#> 4  4    2     1
#> 5  4    6     1
#> 6  4    7     1
#> 7  4    8     0
#> 8  5    3     1
#> 9  5    3     2

# Calculate MCC using the equation method
mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

# Print the S3 object
mcc_eq
#> 
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Dong-Yasui Equation Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time   mcc
#>   <dbl> <dbl>
#> 1     0  0   
#> 2     2  0.25
#> 3     3  0.5 
#> 4     6  0.75
#> 5     7  1   
#> ── Call ──
#> 
#> mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause")

# Get summary
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

# Extract MCC estimates
mcc_estimates(mcc_eq)
#> # A tibble: 5 × 2
#>    time   mcc
#>   <dbl> <dbl>
#> 1     0  0   
#> 2     2  0.25
#> 3     3  0.5 
#> 4     6  0.75
#> 5     7  1   

# Extract calculation details
mcc_details(mcc_eq)
#> # A tibble: 9 × 8
#>    time nrisk censor event cmprk overall_surv_previous ave_events   mcc
#>   <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
#> 1  0        5      0     0     0                  1          0     0   
#> 2  1        5      1     0     0                  1          0     0   
#> 3  2        4      0     1     0                  1          0.25  0.25
#> 4  3        4      0     1     0                  1          0.25  0.5 
#> 5  3.00     4      0     0     1                  1          0     0.5 
#> 6  5        3      0     0     1                  0.75       0     0.5 
#> 7  6        2      0     1     0                  0.5        0.25  0.75
#> 8  7        2      0     1     0                  0.5        0.25  1   
#> 9  8        2      2     0     0                  0.5        0     1   

# Calculate MCC using the sum of cumulative incidence method
mcc_sci <- mcc(
  df,
  id_var = "id",
  time_var = "time",
  cause_var = "cause",
  method = "sci"
)
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

mcc_sci
#> 
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Sum of Cumulative Incidence Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time SumCIs
#>   <dbl>  <dbl>
#> 1     0   0   
#> 2     2   0.25
#> 3     3   0.5 
#> 4     6   0.75
#> 5     7   1   
#> ── Call ──
#> 
#> mcc(data = df, id_var = "id", time_var = "time", cause_var = "cause", 
#>     method = "sci")

# Clean up
rm(df, mcc_eq, mcc_sci)
```
