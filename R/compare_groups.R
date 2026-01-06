#' Compare MCC Between Groups
#'
#' @description
#' Create pairwise comparisons of mean cumulative count (MCC) between groups
#' using mean cumulative count difference (MCCD) and/or mean cumulative count
#' ratio (MCCR). This function only works with grouped `mcc` objects.
#'
#' @details
#' The difference (MCCD) and/or ratio (MCCR) of MCC values are calculated
#' between groups over time. When groups have different maximum follow-up times,
#' MCCD and MCCR are calculated only during the period where both groups have
#' observed data. Beyond this shared period, MCCD and MCCR are set to `NA`
#' For example, if one group has a year of follow-up and the other has 1.5
#' years, then comparisons will only be calculated up to 1 year; since only one
#' group has observed follow-up time beyond that point, comparisons are not
#' possible and the comparison value is set to `NA` for those times. For time
#' points where groups have misaligned observation times, last observation
#' carried forward is used within each group's own follow-up period, providing
#' comparisons across all event times as long as they occur during the shared
#' follow-up period.
#'
#' **Comparison Measures:**
#' - MCCD (difference): `MCC_group1 - MCC_reference`
#' - MCCR (ratio): `MCC_group1 / MCC_reference`
#'
#' @param x (`mcc_grouped`)\cr A grouped `mcc` object (created with `by`
#'     argument in [mcc()])
#' @param reference (`string`, optional)\cr Specifies the reference group(s) to
#'     use when calculating comparisons (see details below to understand how
#'     the `reference` group is used in `measure` calculations. If `NULL`
#'     (default), then the first group alphabetically is used as the
#'     `reference`. When `pairwise = TRUE` and there are > 2 groups being
#'     compared, this argument can accept a vector of group names (in priority
#'     order) to use as the `reference` in each `pairwise` comparison.
#' @param pairwise (`logical`)\cr Whether to perform all possible pairwise
#'     comparisons when > 2 groups are present. If `FALSE` (default),
#'     compares all groups to a single `reference` group only.
#' @param measure (`string`)\cr Specifies which MCC comparison measure(s) to
#'     calculate:`"difference"` (default), `"ratio"`, or `"both"`
#'
#' @returns An S3 object of class `mcc_group_comparison` containing:
#'   - `comparisons`: List of data frames with comparison results over time
#'   - `metadata`: List with comparison parameters and settings
#'   - `original_mcc`: The input `mcc` object
#'   - `call`: The matched call
#'
#' @export
#'
#' @examples
#' # Create sample data with 2 groups
#' library(dplyr)
#' df <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
#'   time = c(8, 7, 5, 2, 6, 7, 8, 3, 4),
#'   cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2),
#'   group = c("A", "A", "B", "B", "B", "B", "B", "A", "A")
#' ) |>
#'   arrange(id, time)
#'
#' # Calculate grouped MCC
#' mcc_grouped <- mcc(df, "id", "time", "cause", by = "group")
#'
#' # Compare groups (difference by default)
#' comparison <- compare_groups(mcc_grouped, reference = "B")
#'
#' # Compare using ratio
#' comparison_ratio <- compare_groups(mcc_grouped, reference = "B", measure = "ratio")
#'
#' # Get both measures
#' comparison_both <- compare_groups(mcc_grouped, reference = "B", measure = "both")
#'
#' # Three group example with pairwise comparisons
#' df3 <- data.frame(
#'   id = c(1, 2, 3, 4, 4, 5, 5, 6, 6, 7, 8, 8, 9),
#'   time = c(8, 7, 5, 6, 7, 7, 8, 3,  4, 4, 5, 6, 6),
#'   cause = c(0, 0, 2, 1, 0, 1, 0, 1, 2, 2, 1, 0, 0),
#'   group = c("A", "A", "A", "B", "B", "B", "B",  "B",  "B", "C", "C", "C", "C")
#' )
#'
#' mcc_3group <- mcc(df3, "id", "time", "cause", by = "group")
#'
#' # All pairwise comparisons
#' comparison_pairwise <- compare_groups(mcc_3group, pairwise = TRUE)
#'
#' # Compare to specific reference
#' comparison_vs_a <- compare_groups(mcc_3group, reference = "A")
#'
#' # Pairwise with reference preference order
#' # Prefers A as reference, then B, then C
#' comparison_pref <- compare_groups(
#'   mcc_3group,
#'   reference = c("A", "B", "C"),
#'   pairwise = TRUE
#' )
#'
compare_groups <- function(
  x,
  reference = NULL,
  pairwise = FALSE,
  measure = c("difference", "ratio", "both")
) {
  # Validate inputs
  validate_comparison_inputs(x, reference, pairwise)

  # Match measure argument
  measure <- match.arg(measure)

  # Get all groups
  all_groups <- mcc_groups(x)

  # Validate and handle reference selection
  reference_info <- validate_reference_selection(
    reference = reference,
    all_groups = all_groups,
    pairwise = pairwise
  )

  # Create comparison pairs
  comparison_pairs <- create_comparison_pairs(
    all_groups = all_groups,
    reference = reference_info$reference,
    pairwise = pairwise
  )

  # Perform comparisons
  comparison_results <- perform_comparisons(
    x = x,
    comparison_pairs = comparison_pairs,
    measure = measure
  )

  # Compile metadata
  metadata <- list(
    reference_group = if (!pairwise) reference_info$reference else NULL,
    reference_preferences = if (pairwise && !is.null(reference))
      reference_info$reference else NULL,
    pairwise = pairwise,
    measure = measure,
    n_comparisons = length(comparison_results$comparisons),
    comparison_pairs = comparison_results$pairs_df,
    weighted = x$weighted,
    method = x$method,
    grouping_var = x$by_group
  )

  # Create output object
  result <- structure(
    list(
      comparisons = comparison_results$comparisons,
      metadata = metadata,
      original_mcc = x,
      call = match.call()
    ),
    class = c("mcc_group_comparison", "list")
  )

  # Issue warning if defaults were used in pairwise comparisons
  if (pairwise && any(comparison_results$pairs_df$default_used)) {
    issue_default_reference_warning(comparison_results$pairs_df)
  }

  return(result)
}
