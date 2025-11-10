# Calculate Mean Cumulative Count using the equation method

Calculate Mean Cumulative Count using the equation method

## Usage

``` r
mcc_equation(
  data,
  id_var,
  time_var,
  cause_var,
  weights = NULL,
  adjust_times = TRUE,
  time_precision = 1e-06,
  include_details = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing the required variables

- id_var:

  Name of the column containing participant IDs (as string or symbol)

- time_var:

  Name of the column containing follow-up times (as string or symbol)

- cause_var:

  Name of the column containing event indicators (as string or symbol)
  (1=event of interest, 2=competing risk, 0=censoring)

- weights:

  Name of the column containing weights (as string, optional)

- adjust_times:

  Whether to automatically adjust times for simultaneous events
  (default: TRUE)

- time_precision:

  Precision used for adjusting simultaneous events (default: 1e-6)

- include_details:

  Whether to include detailed calculation tables and intermediate
  objects in the output (default: TRUE).

## Value

A list containing MCC results. If `include_details = TRUE`, returns
complete calculation details. Otherwise, returns only the final MCC
estimates.
