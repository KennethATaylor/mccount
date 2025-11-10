# Package index

## Calculate MCC

- [`mcc()`](https://kennethataylor.github.io/mccount/reference/mcc.md) :
  Calculate Mean Cumulative Count (MCC)

## Examine Results

- [`print(`*`<mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/print.mcc.md)
  :

  Print method for `mcc` objects

- [`summary(`*`<mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/summary.mcc.md)
  :

  Summary method for `mcc` objects

- [`print(`*`<summary.mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/print.summary.mcc.md)
  :

  Print method for `mcc` summary objects

## Visualize Results

- [`plot(`*`<mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/plot.mcc.md)
  : Plot MCC results

- [`autoplot(`*`<mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/autoplot.mcc.md)
  :

  Auto-plot method for `mcc` objects

- [`geom_line_mcc()`](https://kennethataylor.github.io/mccount/reference/geom_line_mcc.md)
  : Add Reference Lines at an MCC Threshold to ggplot2 Objects

## S3 Object Utilities

### Extract results

- [`mcc_details()`](https://kennethataylor.github.io/mccount/reference/mcc_details.md)
  :

  Extract calculation details from `mcc` objects

- [`mcc_estimates()`](https://kennethataylor.github.io/mccount/reference/mcc_estimates.md)
  :

  Extract MCC estimates from `mcc` objects

- [`mcc_final_values()`](https://kennethataylor.github.io/mccount/reference/mcc_final_values.md)
  : Get final MCC value for each group

- [`mcc_method()`](https://kennethataylor.github.io/mccount/reference/mcc_method.md)
  : Get the method used for MCC calculation

### Test, convert, and compare MCC objects

- [`as_mcc()`](https://kennethataylor.github.io/mccount/reference/as_mcc.md)
  :

  Convert objects to `mcc` class

- [`as.data.frame(`*`<mcc>`*`)`](https://kennethataylor.github.io/mccount/reference/as.data.frame.mcc.md)
  :

  Convert `mcc` object to data.frame

- [`is_mcc()`](https://kennethataylor.github.io/mccount/reference/is_mcc.md)
  :

  Check if object is an `mcc` result

- [`is_weighted()`](https://kennethataylor.github.io/mccount/reference/is_weighted.md)
  :

  Check if `mcc` object uses weighted estimation

- [`compare_mcc()`](https://kennethataylor.github.io/mccount/reference/compare_mcc.md)
  :

  Compare `mcc` objects

- [`print(`*`<mcc_comparison>`*`)`](https://kennethataylor.github.io/mccount/reference/print.mcc_comparison.md)
  : Print method for MCC comparison objects

### Group/strata related

- [`is_grouped()`](https://kennethataylor.github.io/mccount/reference/is_grouped.md)
  :

  Check if `mcc` object is from grouped analysis

- [`mcc_groups()`](https://kennethataylor.github.io/mccount/reference/mcc_groups.md)
  :

  Extract unique groups from grouped `mcc` object

- [`mcc_grouping_var()`](https://kennethataylor.github.io/mccount/reference/mcc_grouping_var.md)
  :

  Get grouping variable name from grouped `mcc` object

- [`filter_mcc()`](https://kennethataylor.github.io/mccount/reference/filter_mcc.md)
  :

  Filter `mcc` object by groups
