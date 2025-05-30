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
#' @param by (`string`, optional)\cr
#'     Name of the column to group by for calculating MCC within subgroups.
#'     If provided, MCC will be calculated separately for each level of this
#'     variable
#' @param method (`string`)\cr
#'     Method to use for MCC calculation. Either `"equation"` (default) or
#'     `"sci"` (sum of cumulative incidence)
#' @param tstart_var (`string`)\cr
#'     Name of the column containing start times of follow-up for incorporating
#'     left truncation. Only allowed to be specified when `method = "sci"`. If
#'     `NULL` (default), a constant value of `0` is used in calculation (i.e.,
#'     right truncation only)
#' @param adjust_times (`logical`)\cr
#'     If `TRUE` (default), automatically adjusts times to account for outcome
#'     events and competing risk events occurring at the same time
#' @param time_precision (`numeric`)\cr
#'     Precision used for adjusting simultaneous events (default: 1e-6). Must
#'     be a positive numeric value
#' @param include_details (`logical`)\cr
#'     Whether to include detailed calculation tables and intermediate objects
#'     in the output. Default is `TRUE`, which returns all calculation details.
#'     Setting to `FALSE` returns only the final MCC estimates, making the
#'     function more efficient for bootstrapping
#' @param calculate_se (`logical`)\cr
#'     Whether to calculate analytical standard errors and confidence intervals.
#'     Only available for `method = "equation"`. Default is `FALSE`
#' @param se_method (`string`)\cr
#'     Method for calculating confidence intervals when `calculate_se = TRUE`.
#'     Options are `"normal"` for normal approximation or `"log"` (default) for
#'     log-transformation which typically provides better coverage properties
#' @param conf_level (`numeric`)\cr
#'     Confidence level for confidence intervals when `calculate_se = TRUE`.
#'     Must be between 0 and 1. Default is 0.95 (95% confidence intervals)
#'
#' @references
#' Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
#'     Estimating the burden of recurrent events in the presence of competing
#'     risks: the method of mean cumulative count. *Am J Epidemiol*. 2015 Apr
#'     1;181(7):532-40. doi: [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)
#'
#' Ghosh D, Lin DY. Nonparametric analysis of recurrent events and death.
#'     *Biometrics*. 2000;56(2):554-562. doi: [10.1111/j.0006-341X.2000.00554.x](https://doi.org/10.1111/j.0006-341X.2000.00554.x)
#'
#' @returns
#' When `by` is NULL (single group analysis):
#'
#' When `include_details = TRUE` (default), a list with method-specific components:
#'
#' For `method = "equation"`:
#' * `mcc_final`: A tibble with columns for `time` and `mcc`. If `calculate_se = TRUE`,
#'   also includes `se`, `lower_ci`, `upper_ci`, `ci_method`, and `conf_level`
#' * `mcc_table`: A tibble with detailed calculation steps. If `calculate_se = TRUE`,
#'   also includes standard error and confidence interval columns
#' * `original_data`: The input `data` with standardized column names
#' * `adjusted_data`: Present only if time adjustments were applied
#' * `se_info`: Present only if `calculate_se = TRUE`, contains metadata about SE calculation
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
#' * `se_info`: Present only if `calculate_se = TRUE`
#' * `method`: The method used for calculation
#'
#' When `by` is specified (grouped analysis):
#'
#' A list with the same structure as above, but with an additional `by_group`
#' component and all tibbles containing an additional column with the grouping
#' variable values. The `by_group` component contains the name of the grouping
#' variable for reference.
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
#' # Calculate MCC with analytical standard errors
#' mcc_with_se <- mcc(
#'   df,
#'   id_var = "id",
#'   time_var = "time",
#'   cause_var = "cause",
#'   calculate_se = TRUE,
#'   se_method = "log"
#' )
#'
#' # View results with confidence intervals
#' mcc_with_se$mcc_final
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
#' rm(mcc_with_se)
#' rm(mcc_sci)
#'
#' @export
mcc <- function(
  data,
  id_var,
  time_var,
  cause_var,
  by = NULL,
  method = c("equation", "sci"),
  tstart_var = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE,
  calculate_se = FALSE,
  se_method = c("log", "normal"),
  conf_level = 0.95
) {
  # Match and validate method argument
  method <- match.arg(method)
  se_method <- match.arg(se_method)

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

  # Validate calculate_se is logical
  if (!is.logical(calculate_se) || length(calculate_se) != 1) {
    cli::cli_abort(c(
      "{.arg calculate_se} must be a {.cls logical} value (`TRUE` or `FALSE`)",
      "x" = "Received: {.val {calculate_se}}"
    ))
  }

  # Check if calculate_se is requested with unsupported method
  if (calculate_se && method != "equation") {
    cli::cli_abort(c(
      "Analytical standard errors are only available for {.code method = \"equation\"}",
      "i" = "You specified {.code method = \"{method}\"} with {.code calculate_se = TRUE}",
      "i" = "Either change to {.code method = \"equation\"} or set {.code calculate_se = FALSE}"
    ))
  }

  # Validate conf_level
  if (calculate_se) {
    if (
      !is.numeric(conf_level) ||
        length(conf_level) != 1 ||
        conf_level <= 0 ||
        conf_level >= 1
    ) {
      cli::cli_abort(c(
        "{.arg conf_level} must be a single number between 0 and 1",
        "x" = "Received: {.val {conf_level}}"
      ))
    }
  }

  # Validate by argument if provided
  if (!is.null(by)) {
    validate_by_variable(data, by)
  }

  # Handle grouped vs ungrouped analysis
  if (is.null(by)) {
    # Single group analysis (existing behavior - call internal functions directly)
    if (method == "equation") {
      if (calculate_se) {
        result <- mcc_equation_with_se(
          data = data,
          id_var = {{ id_var }},
          time_var = {{ time_var }},
          cause_var = {{ cause_var }},
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details,
          calculate_se = calculate_se,
          se_method = se_method,
          conf_level = conf_level
        )
      } else {
        result <- mcc_equation(
          data = data,
          id_var = {{ id_var }},
          time_var = {{ time_var }},
          cause_var = {{ cause_var }},
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details
        )
      }
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
  } else {
    # Grouped analysis
    result <- mcc_by_group(
      data = data,
      id_var = {{ id_var }},
      time_var = {{ time_var }},
      cause_var = {{ cause_var }},
      by = by,
      method = method,
      tstart_var = {{ tstart_var }},
      adjust_times = adjust_times,
      time_precision = time_precision,
      include_details = include_details,
      calculate_se = calculate_se,
      se_method = se_method,
      conf_level = conf_level
    )
  }

  return(result)
}

#' Calculate MCC by group (internal function)
#'
#' @inheritParams mcc
#' @param by Name of grouping variable
#' @keywords internal
#' @noRd
mcc_by_group <- function(
  data,
  id_var,
  time_var,
  cause_var,
  by,
  method,
  tstart_var = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE,
  calculate_se = FALSE,
  se_method = "log",
  conf_level = 0.95
) {
  # Convert to symbols for consistent handling
  id_var_sym <- rlang::ensym(id_var)
  time_var_sym <- rlang::ensym(time_var)
  cause_var_sym <- rlang::ensym(cause_var)

  if (!is.null(tstart_var)) {
    tstart_var_sym <- rlang::ensym(tstart_var)
  } else {
    tstart_var_sym <- NULL
  }

  # Get unique groups efficiently
  unique_groups <- unique(data[[by]])
  unique_groups <- unique_groups[!is.na(unique_groups)] # Remove NA values

  if (length(unique_groups) == 0) {
    cli::cli_abort(c(
      "No valid groups found in {.arg by} variable",
      "x" = "All values in column '{by}' are NA"
    ))
  }

  # Pre-allocate list for results
  group_results <- vector("list", length(unique_groups))
  names(group_results) <- as.character(unique_groups)

  # Calculate MCC for each group
  for (i in seq_along(unique_groups)) {
    group_val <- unique_groups[i]
    group_data <- data[data[[by]] == group_val & !is.na(data[[by]]), ]

    if (nrow(group_data) == 0) {
      cli::cli_warn("No data available for group: {.val {group_val}}")
      next
    }

    # Calculate MCC for this group using the internal functions directly
    if (method == "equation") {
      if (calculate_se) {
        group_result <- mcc_equation_with_se(
          data = group_data,
          id_var = !!id_var_sym,
          time_var = !!time_var_sym,
          cause_var = !!cause_var_sym,
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details,
          calculate_se = calculate_se,
          se_method = se_method,
          conf_level = conf_level
        )
      } else {
        group_result <- mcc_equation(
          data = group_data,
          id_var = !!id_var_sym,
          time_var = !!time_var_sym,
          cause_var = !!cause_var_sym,
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details
        )
      }
    } else if (method == "sci") {
      # Handle tstart_var conditionally to avoid NULL issues
      if (!is.null(tstart_var_sym)) {
        group_result <- mcc_sci(
          data = group_data,
          id_var = !!id_var_sym,
          time_var = !!time_var_sym,
          cause_var = !!cause_var_sym,
          tstart_var = tstart_var_sym,
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details
        )
      } else {
        group_result <- mcc_sci(
          data = group_data,
          id_var = !!id_var_sym,
          time_var = !!time_var_sym,
          cause_var = !!cause_var_sym,
          adjust_times = adjust_times,
          time_precision = time_precision,
          include_details = include_details
        )
      }
    }

    # Add group identifier to all tibbles in the result
    group_result <- add_group_column_to_result(group_result, by, group_val)

    group_results[[as.character(group_val)]] <- group_result
  }

  # Remove any NULL results (from empty groups)
  group_results <- group_results[!sapply(group_results, is.null)]

  if (length(group_results) == 0) {
    cli::cli_abort("No valid results obtained for any group")
  }

  # Combine results from all groups
  combined_result <- combine_group_results(group_results, by, include_details)

  # Add method and by_group to the result
  combined_result$method <- method
  combined_result$by_group <- by

  return(combined_result)
}
