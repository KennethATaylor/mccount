#' Calculate Mean Cumulative Count (MCC)
#'
#' @description
#' Calculates the Mean Cumulative Count (MCC), which estimates the expected
#' cumulative number of events per person over time, while accounting for
#' potential competing risks and censoring. This function provides a unified
#' interface to two different estimation approaches: the `"equation"` method and
#' the sum of cumulative incidence (`"sci"`) method.
#'
#' The `"equation"` method calculates MCC directly through probability
#' calculations, while the `"sci"` method derives MCC by summing the cumulative
#' incidence functions for each recurrent event. The two approaches yield
#' equivalent results in certain circumstances. When they do not, the choice
#' between methods depends on the specific outcome, analysis needs, and data
#' structure.
#'
#' @param data (`data.frame` or `tbl_df`)\cr
#'     A `data.frame` or tibble containing the required variables
#' @param id_var (`string`)\cr
#'     Name of the column containing participant identifiers
#' @param time_var  (`string`)\cr
#'     Name of the column containing follow-up times
#' @param cause_var (`string`)\cr
#'     Name of the column containing event indicator values (1 = event of
#'     interest, 2 = competing risk, 0 = censoring)
#' @param method (`string`)\cr
#'     Method to use for MCC calculation. Either `"equation"` (default) or
#'     `"sci"` (sum of cumulative incidence)
#' @param tstart_var (`string`)\cr
#'     Name of the column containing start times of follow-up for incorporating
#'     left truncation. Only allowed to be specified when `method = "sci"`. If
#'     `NULL` (default), a constant value of `0` is used in calculation (i.e.,
#'     right truncation only).
#' @param adjust_times (`logical`)\cr
#'     If `TRUE` (default), automatically adjusts times to account for outcome
#'     events and competing risk events occurring at the same time.
#' @param time_precision (`numeric`)\cr
#'     Precision used for adjusting simultaneous events (default: 1e-6). Must
#'     be a positive numeric value.
#' @param include_details (`logical`)\cr
#'     Whether to include detailed calculation tables and intermediate objects
#'     in the output. Default is `TRUE`, which returns all calculation details.
#'     Setting to `FALSE` returns only the final MCC estimates, making the function
#'     more efficient for bootstrapping.
#'
#' @references
#' Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
#'     Estimating the burden of recurrent events in the presence of competing
#'     risks: the method of mean cumulative count. *Am J Epidemiol*. 2015 Apr
#'     1;181(7):532-40. doi: [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)
#'
#' @returns
#' When `include_details = TRUE` (default), a list with method-specific components:
#'
#' For `method = "equation"`:
#' * `mcc_final`: A tibble with columns for `time` and `mcc`
#' * `mcc_table`: A tibble with detailed calculation steps
#' * `original_data`: The input `data` with standardized column names
#' * `adjusted_data`: Present only if time adjustments were applied
#' * `method`: The method used for calculation
#'
#' For `method = "sci"`:
#' * `mcc_final`: A tibble with columns for `time` and MCC (expressed as `SumCIs`)
#' * `sci_table`: A tibble with cumulative incidence for each event number and their sum
#' * `all_cis`: A list of cumulative incidence data for each event number
#' * `mcc_base`: A tibble with calculation details for the MCC
#' * `original_data`: The input `data` with standardized column names
#' * `adjusted_data`: Present only if time adjustments were applied
#' * `method`: The `method` used for calculation
#'
#' When `include_details = FALSE`, a simplified list containing only:
#' * `mcc_final`: A tibble with columns for `time` and `mcc` (or `SumCIs` for `method = "sci"`)
#' * `method`: The method used for calculation
#'
#' @examples
#' # Attach dplyr
#' library(dplyr)
#' # Create sample data with recurrent events
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 5, 5),
#'   time = c(8, 1, 5, 2, 6, 7, 3, 3), # Times will be adjusted for id = 5
#'   cause = c(0, 0, 2, 1, 1, 1, 1, 2)
#'  ) |>
#'   arrange(id, time)  # Sort the data by id and time
#'
#' # Print the dataset
#' print("Hypothetical dataset from Dong et al. (2015):")
#' print(df)
#'
#' # Calculate MCC using the equation method (default)
#' mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#'
#' # MCC table
#' mcc_eq$mcc_table
#'
#' # MCC estimates
#' mcc_eq$mcc_final
#'
#' # Calculate MCC using the sum of cumulative incidence method
#' mcc_sci <- mcc(
#'   df,
#'   id_var = "id",
#'   time_var = "time",
#'   cause_var = "cause",
#'   method = "sci"
#' )
#'
#' # MCC (SCI) table
#' mcc_sci$sci_table
#'
#' # MCC estimates
#' mcc_sci$mcc_final
#'
#' # Clean up
#' rm(df)
#' rm(mcc_eq)
#' rm(mcc_sci)
#'
#' @export
mcc <- function(
  data,
  id_var,
  time_var,
  cause_var,
  method = c("equation", "sci"),
  tstart_var = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE
) {
  # Match and validate method argument
  method <- match.arg(method)

  # Validate time_precision
  if (
    !is.numeric(time_precision) ||
      length(time_precision) != 1 ||
      time_precision <= 0
  ) {
    cli::cli_abort(c(
      "{.arg time_precision} must be a positive numeric value",
      "x" = "Received: {.val {time_precision}}"
    ))
  }

  # Check if tstart_var is provided with method "equation"
  if (
    !is.null(rlang::enquo(tstart_var)) &&
      !rlang::quo_is_null(rlang::enquo(tstart_var)) &&
      method == "equation"
  ) {
    cli::cli_abort(c(
      "{.arg tstart_var} is only compatible with {.code method = \"sci\"}",
      "i" = "You specified {.code method = \"equation\"}, which does not support start times",
      "i" = "Either change to {.code method = \"sci\"} or remove the {.arg tstart_var} parameter"
    ))
  }

  # Validate adjust_times is logical
  if (!is.logical(adjust_times) || length(adjust_times) != 1) {
    cli::cli_abort(c(
      "{.arg adjust_times} must be a {.cls logical} value (`TRUE` or `FALSE`)",
      "x" = "Received: {.val {adjust_times}}"
    ))
  }

  # Validate include_details is logical
  if (!is.logical(include_details) || length(include_details) != 1) {
    cli::cli_abort(c(
      "{.arg include_details} must be a {.cls logical} value (`TRUE` or `FALSE`)",
      "x" = "Received: {.val {include_details}}"
    ))
  }

  # Dispatch to appropriate internal function based on method
  if (method == "equation") {
    result <- mcc_equation(
      data = data,
      id_var = {{ id_var }},
      time_var = {{ time_var }},
      cause_var = {{ cause_var }},
      adjust_times = adjust_times,
      time_precision = time_precision,
      include_details = include_details
    )
  } else if (method == "sci") {
    result <- mcc_sci(
      data = data,
      id_var = {{ id_var }},
      time_var = {{ time_var }},
      cause_var = {{ cause_var }},
      tstart_var = {{ tstart_var }},
      adjust_times = adjust_times,
      time_precision = time_precision,
      include_details = include_details
    )
  }

  # Add method used to the result
  result$method <- method

  return(result)
}
