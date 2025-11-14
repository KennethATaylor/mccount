# Calculate Mean Cumulative Count using the Sum of Cumulative Incidence method

Calculate Mean Cumulative Count using the Sum of Cumulative Incidence
method

## Usage

``` r
mcc_sci(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL,
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

  Name of the column containing follow-up or event times (as string or
  symbol)

- cause_var:

  Name of the column containing event indicators (as string or symbol)
  (1=event of interest, 2=competing risk, 0=censoring)

- tstart_var:

  Name of the column containing start times of follow-up (as string or
  symbol, optional). If NULL (default), a constant value of 0 is used
  for all observations.

- adjust_times:

  Whether to automatically adjust times for simultaneous events
  (default: TRUE)

- time_precision:

  Precision used for adjusting simultaneous events (default: 1e-6)

- include_details:

  Whether to include detailed calculation tables and intermediate
  objects in the output (default: TRUE).

## Value

A list containing MCC results. If include_details=TRUE, returns complete
calculation details. Otherwise, returns only the final MCC estimates.
