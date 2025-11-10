# Get Time When MCC Reaches a Specific Threshold

Helper function that identifies the first time point when the Mean
Cumulative Count (MCC) reaches or exceeds the threshold. An MCC value of
the threshold represents the time when the population experiences an
average of `<threshold>` event(s).

## Usage

``` r
get_time_to_mcc(mcc_data, mcc_column, threshold = 1)
```

## Arguments

- mcc_data:

  A data frame containing MCC estimates over time. This is typically the
  `mcc_final` component from an `mcc` object.

- mcc_column:

  A string specifying the name of the column containing MCC values. For
  `method = "equation"`, this is typically `"mcc"`. For
  `method = "sci"`, this is typically `"SumCIs"`.

- threshold:

  numeric;determines MCC value threshold to use (default = `1.0`)

## Value

A numeric value representing the time when MCC first reaches or exceeds
the `threshold`, or `NA_real_` if MCC never reaches `threshold` during
the observed follow-up period.

## Details

The MCC represents the expected cumulative number of events per person
in the population initially at risk. When MCC = `threshold`, this
indicates that the population has experienced an average of 1 event per
person. This milestone can be useful for:

- Identifying when the event burden reaches clinical or epidemiological
  significance

- Comparing event timing across different exposure groups or populations

- Setting thresholds for intervention planning

Note that MCC values can exceed `threshold`, indicating more than
`threshold` number of events per person on average, which distinguishes
it from probability-based measures like cumulative incidence that are
bounded between 0 and 1.
