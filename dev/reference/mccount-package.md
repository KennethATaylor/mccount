# mccount: Estimate Recurrent Event Burden with Competing Risks

Calculates mean cumulative count (MCC) to estimate the expected
cumulative number of recurrent events per person over time in the
presence of competing risks and censoring. Implements both the
Dong-Yasui equation method and sum of cumulative incidence method
described in Dong, et al. (2015)
[doi:10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289) . Supports
inverse probability weighting for causal inference as outlined in Gaber,
et al. (2023)
[doi:10.1093/aje/kwad031](https://doi.org/10.1093/aje/kwad031) .
Provides S3 methods for printing, summarizing, plotting, and extracting
results. Handles grouped analyses and integrates with 'ggplot2'
<https://ggplot2.tidyverse.org/> for visualization.

## Main Function

- [`mcc()`](https://kennethataylor.github.io/mccount/dev/reference/mcc.md) -
  estimates the MCC

## S3 Object System

The package uses S3 classes to provide a consistent, extensible
interface:

**Base Class:**

- `mcc` - All MCC results inherit from this class

**Method-Specific Classes:**

- `mcc_equation` - Results from Dong-Yasui estimator

- `mcc_sci` - Results from the Sum of Cumulative Incidence estimator

**Analysis-Type Classes:**

- `mcc_weighted` - Results using weighting

- `mcc_grouped` - Results from grouped/stratified analysis

Classes combine hierarchically (e.g.,
`c("mcc_grouped", "mcc_weighted", "mcc_equation", "mcc")`).

## Available Methods

**Generic S3 Methods:**

- [`print.mcc()`](https://kennethataylor.github.io/mccount/dev/reference/print.mcc.md) -
  Formatted display of results

- [`summary.mcc()`](https://kennethataylor.github.io/mccount/dev/reference/summary.mcc.md) -
  Statistical summaries

- [`plot.mcc()`](https://kennethataylor.github.io/mccount/dev/reference/plot.mcc.md) -
  Visualization with ggplot2

- [`autoplot.mcc()`](https://kennethataylor.github.io/mccount/dev/reference/autoplot.mcc.md) -
  ggplot2-style plotting (when ggplot2 loaded)

- [`as.data.frame.mcc()`](https://kennethataylor.github.io/mccount/dev/reference/as.data.frame.mcc.md) -
  Convert to standard data.frame

- [`as_mcc()`](https://kennethataylor.github.io/mccount/dev/reference/as_mcc.md) -
  Convert other objects to MCC class

**Utility Functions:**

- [`is_mcc()`](https://kennethataylor.github.io/mccount/dev/reference/is_mcc.md) -
  Test if object is MCC result

- [`mcc_estimates()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_estimates.md) -
  Extract main results table

- [`mcc_details()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_details.md) -
  Extract calculation details

- [`mcc_method()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_method.md) -
  Get calculation method used

- [`is_weighted()`](https://kennethataylor.github.io/mccount/dev/reference/is_weighted.md),
  [`is_grouped()`](https://kennethataylor.github.io/mccount/dev/reference/is_grouped.md) -
  Check analysis properties

- [`mcc_groups()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_groups.md),
  [`mcc_grouping_var()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_grouping_var.md) -
  Access grouping information

- [`filter_mcc()`](https://kennethataylor.github.io/mccount/dev/reference/filter_mcc.md) -
  Filter grouped results

- [`mcc_final_values()`](https://kennethataylor.github.io/mccount/dev/reference/mcc_final_values.md) -
  Extract final MCC values

- [`compare_mcc()`](https://kennethataylor.github.io/mccount/dev/reference/compare_mcc.md) -
  Compare two MCC objects

## Basic Usage

    # Calculate MCC
    result <- mcc(data, "id", "time", "cause")

    # Examine results
    result              # Uses print.mcc()
    summary(result)     # Uses summary.mcc()
    plot(result)        # Uses plot.mcc()

    # Extract components
    estimates <- mcc_estimates(result)
    details <- mcc_details(result)
    final_values <- mcc_final_values(result)

    # Grouped analysis
    grouped_result <- mcc(data, "id", "time", "cause", by = "treatment")
    plot(grouped_result)
    filter_mcc(grouped_result, "Treatment A")

## Plotting

The package provides flexible plotting through S3 methods that
automatically adapt to analysis type:

    # Basic plotting
    plot(mcc_result)                    # MCC over time
    plot(mcc_result, type = "details")  # Calculation components

    # Customization
    plot(mcc_result, colors = c("red", "blue"), title = "Custom Title")

    # ggplot2 integration
    library(ggplot2)
    autoplot(mcc_result) + theme_classic()

    # Further customization
    plot(mcc_result) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      labs(caption = "Dashed line at MCC = 1")

## References

**Core Methods:** Dong H, Robison LL, Leisenring WM, Martin LJ,
Armstrong GT, Yasui Y. Estimating the burden of recurrent events in the
presence of competing risks: the method of mean cumulative count. *Am J
Epidemiol*. 2015;181(7):532-40.

**Weighted Extension:** Gaber CE, Edwards JK, Lund JL, Peery AF,
Richardson DB, Kinlaw AC. Inverse Probability Weighting to Estimate
Exposure Effects on the Burden of Recurrent Outcomes in the Presence of
Competing Events. *Am J Epidemiol*. 2023;192(5):830-839.

## See also

Useful links:

- <https://github.com/KennethATaylor/mccount>

- <https://kennethataylor.github.io/mccount/>

- Report bugs at <https://github.com/KennethATaylor/mccount/issues>

## Author

**Maintainer**: Kenneth A. Taylor <kenneth.taylor.dpt@gmail.com>
([ORCID](https://orcid.org/0000-0002-3205-9280)) \[copyright holder\]
