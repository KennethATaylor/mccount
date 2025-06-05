#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom rlang .data is_true is_logical set_names as_function global_env inject "%||%"
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' @section Main Function:
#' * [mcc()] - estimates the MCC
#'
#' @section S3 Object System:
#' The package uses S3 classes to provide a consistent, extensible interface:
#'
#' **Base Class:**
#' * `mcc` - All MCC results inherit from this class
#'
#' **Method-Specific Classes:**
#' * `mcc_equation` - Results from Dong-Yasui estimator
#' * `mcc_sci` - Results from the Sum of Cumulative Incidence estimator
#'
#' **Analysis-Type Classes:**
#' * `mcc_weighted` - Results using weighting
#' * `mcc_grouped` - Results from grouped/stratified analysis
#'
#' Classes combine hierarchically (e.g., `c("mcc_grouped", "mcc_weighted", "mcc_equation", "mcc")`).
#'
#' @section Available Methods:
#'
#' **Generic S3 Methods:**
#' * [print.mcc()] - Formatted display of results
#' * [summary.mcc()] - Statistical summaries
#' * [plot.mcc()] - Visualization with ggplot2
#' * [autoplot.mcc()] - ggplot2-style plotting (when ggplot2 loaded)
#' * [as.data.frame.mcc()] - Convert to standard data.frame
#' * [as_mcc()] - Convert other objects to MCC class
#'
#' **Utility Functions:**
#' * [is_mcc()] - Test if object is MCC result
#' * [mcc_estimates()] - Extract main results table
#' * [mcc_details()] - Extract calculation details
#' * [mcc_method()] - Get calculation method used
#' * [is_weighted()], [is_grouped()] - Check analysis properties
#' * [mcc_groups()], [mcc_grouping_var()] - Access grouping information
#' * [subset_mcc()] - Filter grouped results
#' * [mcc_final_values()] - Extract final MCC values
#' * [compare_mcc()] - Compare two MCC objects
#'
#' @section Basic Usage:
#' ```r
#' # Calculate MCC
#' result <- mcc(data, "id", "time", "cause")
#'
#' # Examine results
#' result              # Uses print.mcc()
#' summary(result)     # Uses summary.mcc()
#' plot(result)        # Uses plot.mcc()
#'
#' # Extract components
#' estimates <- mcc_estimates(result)
#' details <- mcc_details(result)
#' final_values <- mcc_final_values(result)
#'
#' # Grouped analysis
#' grouped_result <- mcc(data, "id", "time", "cause", by = "treatment")
#' plot(grouped_result)
#' subset_mcc(grouped_result, "Treatment A")
#' ```
#'
#' @section Plotting:
#' The package provides flexible plotting through S3 methods that automatically
#' adapt to analysis type:
#'
#' ```r
#' # Basic plotting
#' plot(mcc_result)                    # MCC over time
#' plot(mcc_result, type = "details")  # Calculation components
#'
#' # Customization
#' plot(mcc_result, colors = c("red", "blue"), title = "Custom Title")
#'
#' # ggplot2 integration
#' library(ggplot2)
#' autoplot(mcc_result) + theme_classic()
#'
#' # Further customization
#' plot(mcc_result) +
#'   geom_hline(yintercept = 1, linetype = "dashed") +
#'   labs(caption = "Dashed line at MCC = 1")
#' ```
#'
#' @section References:
#'
#' **Core Methods:**
#' Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
#' Estimating the burden of recurrent events in the presence of competing risks:
#' the method of mean cumulative count. *Am J Epidemiol*. 2015;181(7):532-40.
#'
#' **Weighted Extension:**
#' Gaber CE, Edwards JK, Lund JL, Peery AF, Richardson DB, Kinlaw AC.
#' Inverse Probability Weighting to Estimate Exposure Effects on the Burden of
#' Recurrent Outcomes in the Presence of Competing Events. *Am J Epidemiol*.
#' 2023;192(5):830-839.
#'
#' @name mccount-package
#' @aliases mccount
NULL
