# Convert objects to `mcc` class

Converts objects to MCC class. This is useful when you have calculation
results from other sources that you want to treat as MCC objects.

## Usage

``` r
as_mcc(x, method, weighted = FALSE, by_group = NULL, call = NULL, ...)
```

## Arguments

- x:

  Object to convert to `mcc`

- method:

  Method used for calculation ("equation" or "sci")

- weighted:

  Logical indicating if weighted estimation was used

- by_group:

  Optional name of grouping variable

- call:

  Optional function call to store

- ...:

  Additional arguments (currently unused)

## Value

An `mcc` S3 object

## Examples

``` r
# Convert a data.frame to MCC object
library(dplyr)

# Create a simple data.frame with MCC results
mcc_data <- data.frame(
  time = c(1, 2, 3, 4, 5),
  mcc = c(0.1, 0.3, 0.5, 0.7, 1.0)
)

# Convert to MCC object (equation method)
mcc_obj <- as_mcc(mcc_data, method = "equation")
print(mcc_obj)
#> 
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Dong-Yasui Equation Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time   mcc
#>   <dbl> <dbl>
#> 1     1   0.1
#> 2     2   0.3
#> 3     3   0.5
#> 4     4   0.7
#> 5     5   1  
is_mcc(mcc_obj)  # TRUE
#> [1] TRUE

# Convert for SCI method (requires SumCIs column)
sci_data <- data.frame(
  time = c(1, 2, 3, 4, 5),
  SumCIs = c(0.1, 0.3, 0.5, 0.7, 1.0)
)

mcc_sci_obj <- as_mcc(sci_data, method = "sci")
print(mcc_sci_obj)
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Sum of Cumulative Incidence Method
#> 
#> ── MCC Estimates ──
#> 
#> # A tibble: 5 × 2
#>    time SumCIs
#>   <dbl>  <dbl>
#> 1     1    0.1
#> 2     2    0.3
#> 3     3    0.5
#> 4     4    0.7
#> 5     5    1  

# Convert a list to MCC object
mcc_list <- list(
  mcc_final = data.frame(
    time = c(1, 2, 3),
    mcc = c(0.2, 0.5, 0.8)
  )
)

mcc_from_list <- as_mcc(mcc_list, method = "equation")
print(mcc_from_list)
#> ── Mean Cumulative Count Results ───────────────────────────────────────────────
#> ℹ Method: Dong-Yasui Equation Method
#> 
#> ── MCC Estimates ──
#> 
#>   time mcc
#> 1    1 0.2
#> 2    2 0.5
#> 3    3 0.8

# Clean up
rm(mcc_data, sci_data, mcc_list, mcc_obj, mcc_sci_obj, mcc_from_list)
```
