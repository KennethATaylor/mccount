#' Determine reference for a pair of groups
#' @keywords internal
#' @noRd
determine_pair_reference <- function(pair, reference_prefs) {
  group1 <- pair[1]
  group2 <- pair[2]

  if (is.null(reference_prefs)) {
    # Default to first alphabetically
    ref <- sort(c(group1, group2))[1]
    comp <- setdiff(c(group1, group2), ref)
    default_used <- TRUE
  } else {
    # Find first preference that exists in this pair
    ref_found <- reference_prefs[reference_prefs %in% c(group1, group2)]

    if (length(ref_found) > 0) {
      ref <- ref_found[1] # First match in preference order
      comp <- setdiff(c(group1, group2), ref)
      default_used <- FALSE
    } else {
      # None of the preferences exist in this pair
      ref <- sort(c(group1, group2))[1]
      comp <- setdiff(c(group1, group2), ref)
      default_used <- TRUE
    }
  }

  return(list(ref = ref, comp = comp, default_used = default_used))
}

#' Create comparison pairs
#' @keywords internal
#' @noRd
create_comparison_pairs <- function(all_groups, reference, pairwise) {
  if (!pairwise) {
    # Compare all groups to reference
    other_groups <- setdiff(all_groups, reference)
    pairs <- lapply(other_groups, function(g) {
      list(ref = reference, comp = g, default_used = FALSE)
    })
  } else {
    # Create all pairwise combinations
    group_combinations <- utils::combn(all_groups, 2, simplify = FALSE)

    # Determine reference for each pair
    pairs <- lapply(group_combinations, function(pair) {
      determine_pair_reference(pair, reference)
    })
  }

  return(pairs)
}

#' Get MCC column name based on method
#' @keywords internal
#' @noRd
get_mcc_column_name <- function(mcc_object) {
  if (mcc_object$method == "equation") {
    return("mcc")
  } else {
    return("SumCIs")
  }
}

#' LOCF with NA after a group's max follow-up time (when follow-up times differ)
#' @keywords internal
#' @noRd
locf_with_na_after_max <- function(group_data, all_times, max_time, mcc_col) {
  # For times <= max_time: use LOCF
  # For times > max_time: set to NA

  result <- rep(NA_real_, length(all_times))
  within_followup <- all_times <= max_time

  if (any(within_followup)) {
    locf_fn <- stats::approxfun(
      x = group_data$time,
      y = group_data[[mcc_col]],
      method = "constant",
      rule = 2,
      f = 0,
      ties = "ordered"
    )
    result[within_followup] <- locf_fn(all_times[within_followup])
  }

  return(result)
}

#' Align MCC data to common time grid using LOCF
#' @keywords internal
#' @noRd
align_mcc_times <- function(group1_data, group2_data, truncate_at, mcc_col) {
  # Get max time for each group
  max_time_1 <- max(group1_data$time)
  max_time_2 <- max(group2_data$time)

  # Get union of ALL time points (not just up to truncate_at)
  all_times <- sort(unique(c(group1_data$time, group2_data$time)))

  # LOCF for group 1 (only up to its max time)
  mcc_group1 <- locf_with_na_after_max(
    group1_data,
    all_times,
    max_time_1,
    mcc_col
  )

  # LOCF for group 2 (only up to its max time)
  mcc_group2 <- locf_with_na_after_max(
    group2_data,
    all_times,
    max_time_2,
    mcc_col
  )

  return(data.frame(
    time = all_times,
    mcc_group1 = mcc_group1,
    mcc_group2 = mcc_group2
  ))
}

#' Calculate comparison measures
#' @keywords internal
#' @noRd
calculate_measures <- function(aligned_data, measure, ref_name, comp_name) {
  result <- aligned_data
  result$reference <- ref_name
  result$comparison <- comp_name

  # Calculate difference
  if (measure %in% c("difference", "both")) {
    result$mccd <- result$mcc_group1 - result$mcc_group2
  }

  # Calculate ratio
  if (measure %in% c("ratio", "both")) {
    # Check for zero in denominator
    zero_denom <- result$mcc_group2 == 0
    both_zero <- result$mcc_group1 == 0 & result$mcc_group2 == 0

    # When both are zero, set ratio to 1.0 (for graphing purposes)
    # When only denominator is zero, set to NA
    if (any(zero_denom & !both_zero)) {
      cli::cli_warn(c(
        "!" = "Reference group MCC = 0 at {.val {sum(zero_denom & !both_zero)}} time point{?s}",
        "i" = "MCCR set to {.val NA} at these times",
        "i" = "Comparison: {.val {comp_name}} vs {.val {ref_name}}"
      ))
    }

    # Calculate ratio:
    # - Both zero: set to 1.0
    # - Only denominator zero: set to NA
    # - Otherwise: calculate ratio
    result$mccr <- ifelse(
      both_zero,
      1.0,
      ifelse(zero_denom, NA_real_, result$mcc_group1 / result$mcc_group2)
    )
  }

  # Rename MCC columns for clarity
  names(result)[names(result) == "mcc_group1"] <- paste0("mcc_", comp_name)
  names(result)[names(result) == "mcc_group2"] <- paste0("mcc_", ref_name)

  return(result)
}

#' Perform all comparisons
#' @keywords internal
#' @noRd
perform_comparisons <- function(x, comparison_pairs, measure) {
  mcc_col <- get_mcc_column_name(x)

  # Initialize results
  comparisons <- vector("list", length(comparison_pairs))
  pairs_info <- vector("list", length(comparison_pairs))

  for (i in seq_along(comparison_pairs)) {
    pair <- comparison_pairs[[i]]
    ref_group <- pair$ref
    comp_group <- pair$comp

    table_name <- if (inherits(x, "mcc_sci")) "sci_table" else "mcc_table"

    # Extract data for both groups
    ref_data <- x[[table_name]][x[[table_name]][[x$by_group]] == ref_group, ]
    comp_data <- x[[table_name]][x[[table_name]][[x$by_group]] == comp_group, ]

    # Determine truncation time
    min_time <- min(ref_data$time)
    max_time_ref <- max(ref_data$time)
    max_time_comp <- max(comp_data$time)
    truncate_at <- min(max_time_ref, max_time_comp)

    if (max_time_ref != max_time_comp) {
      # Issue message about truncation
      cli::cli_inform(c(
        "i" = "Max follow-up: {.val {noquote(comp_group)}} = {.val {max_time_comp}}, {.val {noquote(ref_group)}} = {.val {max_time_ref}}",
        "i" = "Valid comparison period: time {.val {min_time}} to {.val {truncate_at}}",
        "i" = "Comparison of {.val {noquote(ref_group)}} and {.val {noquote(comp_group)}} limited to on or before time {.val {truncate_at}} and MCCD/MCCR will be {.val {NA}} beyond that time"
      ))
    }

    # Align times using LOCF
    # Note: comp_group is "group1" and ref_group is "group2" in alignment
    aligned_data <- align_mcc_times(
      group1_data = comp_data,
      group2_data = ref_data,
      truncate_at = truncate_at,
      mcc_col = mcc_col
    )

    # Calculate measures
    comparison_result <- calculate_measures(
      aligned_data = aligned_data,
      measure = measure,
      ref_name = ref_group,
      comp_name = comp_group
    )

    # Store results
    comparisons[[i]] <- comparison_result
    pairs_info[[i]] <- data.frame(
      reference = ref_group,
      comparison = comp_group,
      truncation_time = truncate_at,
      default_used = pair$default_used,
      stringsAsFactors = FALSE
    )
  }

  # Combine pairs info
  pairs_df <- do.call(rbind, pairs_info)

  return(list(
    comparisons = comparisons,
    pairs_df = pairs_df
  ))
}

#' Issue warning about default reference usage
#' @keywords internal
#' @noRd
issue_default_reference_warning <- function(pairs_df) {
  default_comps <- pairs_df[pairs_df$default_used, ]
  comp_labels <- paste0(
    default_comps$comparison,
    " vs ",
    default_comps$reference
  )

  cli::cli_warn(c(
    "!" = "No `reference` group preference found for {.val {nrow(default_comps)}} comparison{?s}",
    "i" = "Using alphabetical default for: {.val {comp_labels}}",
    "i" = "To avoid this, ensure {.arg reference} preferences cover all comparisons"
  ))
}
