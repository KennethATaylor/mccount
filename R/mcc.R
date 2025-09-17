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
#' between methods depe#' Calculate Mean Cumulative Count (MCC)
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
#' @param weights (`string`, optional)\cr
#'     Name of the column containing weights for weighted MCC estimation.
#'     Currently only supported with `method = "equation"`.
#'     If provided, all weights must be non-negative and non-missing
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
#'
#' @references
#' Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
#'     Estimating the burden of recurrent events in the presence of competing
#'     risks: the method of mean cumulative count. *Am J Epidemiol*. 2015 Apr
#'     1;181(7):532-40. doi: [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)
#'
#' Gaber CE, Edwards JK, Lund JL, Peery AF, Richardson DB, Kinlaw AC.
#'     Inverse Probability Weighting to Estimate Exposure Effects on the Burden of
#'     Recurrent Outcomes in the Presence of Competing Events. *Am J Epidemiol*.
#'     2023;192(5):830-839. doi: [10.1093/aje/kwad031](https://doi.org/10.1093/aje/kwad031)
#'
#' @returns
#' An S3 object of class `"mcc"` with method-specific subclasses. The object contains:
#'
#' When `include_details = TRUE` (default):
#'
#' For `method = "equation"`:
#' * `mcc_final`: A tibble with columns for `time` and `mcc`
#' * `mcc_table`: A tibble with detailed calculation steps
#' * `original_data`: The input `data` with standardized column names
#' * `adjusted_data`: Present only if time adjustments were applied
#'
#' For `method = "sci"`:
#' * `mcc_final`: A tibble with columns for `time` and MCC (expressed as `SumCIs`)
#' * `sci_table`: A tibble with cumulative incidence for each event number and their sum
#' * `all_cis`: A list of cumulative incidence data for each event number
#' * `mcc_base`: A tibble with calculation details for the MCC
#' * `original_data`: The input `data` with standardized column names
#' * `adjusted_data`: Present only if time adjustments were applied
#'
#' When `include_details = FALSE`:
#' * `mcc_final`: A tibble with columns for `time` and `mcc` (or `SumCIs` for `method = "sci"`)
#'
#' All objects include metadata:
#' * `method`: The method used for calculation
#' * `weighted`: Logical indicating whether weighted estimation was used
#' * `by_group`: Name of grouping variable (for grouped analyses)
#' * `call`: The original function call
#'
#' When `by` is specified, all tibbles contain an additional column with the
#' grouping variable values, and the object has the additional class `"mcc_grouped"`.
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
#' # Calculate MCC using the equation method
#' mcc_eq <- mcc(df, id_var = "id", time_var = "time", cause_var = "cause")
#'
#' # Print the S3 object
#' mcc_eq
#'
#' # Get summary
#' summary(mcc_eq)
#'
#' # Extract MCC estimates
#' mcc_estimates(mcc_eq)
#'
#' # Extract calculation details
#' mcc_details(mcc_eq)
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
#' mcc_sci
#'
#' # Clean up
#' rm(df, mcc_eq, mcc_sci)
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
  weights = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE
) {
  # Capture the call for the S3 object
  call <- match.call()

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

  # Check if weights are provided with method "sci"
  if (!is.null(weights) && method == "sci") {
    cli::cli_abort(c(
      "{.arg weights} is currently only compatible with {.code method = \"equation\"}",
      "i" = "You specified {.code method = \"sci\"}, which does not currently support weighted estimation",
      "i" = "Either change to {.code method = \"equation\"} or remove the {.arg weights} argument specification"
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

  # Validate by argument if provided
  if (!is.null(by)) {
    validate_by_variable(data, by)
  }

  # Validate weights argument if provided
  if (!is.null(weights)) {
    validate_weights_variable(data, weights)
  }

  # Handle grouped vs ungrouped analysis
  if (is.null(by)) {
    # Single group analysis (existing behavior - call internal functions directly)
    if (method == "equation") {
      result <- mcc_equation(
        data = data,
        id_var = {{ id_var }},
        time_var = {{ time_var }},
        cause_var = {{ cause_var }},
        weights = weights,
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

    # Create S3 object
    mcc_obj <- mcc_object(
      result = result,
      method = method,
      weighted = !is.null(weights),
      by_group = NULL,
      call = call
    )
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
      weights = weights,
      adjust_times = adjust_times,
      time_precision = time_precision,
      include_details = include_details
    )

    # Create S3 object for grouped analysis
    mcc_obj <- mcc_object(
      result = result,
      method = method,
      weighted = !is.null(weights),
      by_group = by,
      call = call
    )
  }

  return(mcc_obj)
}

#' Ensure grouping variable is appropriately typed for analysis
#'
#' @description
#' Converts numeric grouping variables to factors to ensure proper handling
#' in both analysis and plotting. Character variables are left as-is since
#' they will be automatically converted to factors when needed.
#'
#' @param data Input data
#' @param by_var String name of grouping variable
#'
#' @returns Data with potentially modified grouping variable
#' @keywords internal
#' @noRd
prepare_group_variable <- function(data, by_var) {
  if (is.null(by_var)) {
    return(data)
  }

  # Get the grouping column
  group_col <- data[[by_var]]

  # Check if it's numeric (integer or double)
  if (is.numeric(group_col)) {
    # Convert numeric to factor with informative message
    unique_values <- sort(unique(group_col[!is.na(group_col)]))
    n_unique <- length(unique_values)

    cli::cli_inform(c(
      "i" = "Converting numeric grouping variable {.val by_var} to {.cls factor}",
      "i" = "Found {n_unique} unique group{?s}: {.val {noquote(unique_values)}}"
    ))

    # Convert to factor, preserving order of unique values
    data[[by_var]] <- factor(group_col, levels = unique_values)
  }

  return(data)
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
  weights = NULL,
  adjust_times = TRUE,
  time_precision = 1e-6,
  include_details = TRUE
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

  data <- prepare_group_variable(data, by)

  # Get unique groups efficiently
  unique_groups <- unique(data[[by]])
  unique_groups <- unique_groups[!is.na(unique_groups)] # Remove NA values

  if (length(unique_groups) == 0) {
    cli::cli_abort(c(
      "No valid groups found in {.arg by} variable",
      "x" = "All values in column {.val {by}} are {.val {noquote(NA_real_)}}"
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
      group_result <- mcc_equation(
        data = group_data,
        id_var = !!id_var_sym,
        time_var = !!time_var_sym,
        cause_var = !!cause_var_sym,
        weights = weights,
        adjust_times = adjust_times,
        time_precision = time_precision,
        include_details = include_details
      )
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

  # Note: method, weighted, and by_group will be added by the S3 constructor
  # in the main mcc() function, so we don't add them here
  return(combined_result)
}

#' Add group column to result tibbles
#'
#' @param result Result list from MCC calculation
#' @param by_name Name of the grouping variable
#' @param group_value Value of the group
#' @returns Modified result list with group column added
#' @keywords internal
#' @noRd
add_group_column_to_result <- function(result, by_name, group_value) {
  # Components that are tibbles and should get the group column
  tibble_components <- c(
    "mcc_final",
    "mcc_table",
    "sci_table",
    "mcc_base",
    "original_data",
    "adjusted_data"
  )

  for (component in tibble_components) {
    if (
      component %in%
        names(result) &&
        inherits(result[[component]], "data.frame")
    ) {
      # Add group column as the first column
      result[[component]] <- tibble::add_column(
        result[[component]],
        !!rlang::sym(by_name) := group_value,
        .before = 1
      )
    }
  }

  return(result)
}
