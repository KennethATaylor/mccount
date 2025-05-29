test_that("handle_simultaneous_events() works", {
  expect_snapshot(error = TRUE, handle_simultaneous_events())
  expect_snapshot(error = TRUE, handle_simultaneous_events(data_std = 1))
  expect_snapshot(
    error = TRUE,
    handle_simultaneous_events(data_std = 1, adjust_times = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    handle_simultaneous_events(
      data_std = 1,
      adjust_times = TRUE,
      time_precision = 1
    )
  )
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = 1),
    adjust_times = TRUE,
    time_precision = 1
  ))
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = c(1, 1), cause = c(1, 2)),
    adjust_times = TRUE,
    time_precision = 1
  ))
  expect_snapshot(handle_simultaneous_events(
    data_std = data.frame(id = 1, time = c(1, 1), cause = c(1, 2)),
    adjust_times = FALSE,
    time_precision = 1
  ))
})

test_that("add_group_column_to_result() works with mcc_equation results", {
  # Create a mock result from mcc_equation
  mock_result <- list(
    mcc_final = tibble::tibble(
      time = c(5, 10, 15),
      mcc = c(0.1, 0.3, 0.5)
    ),
    mcc_table = tibble::tibble(
      time = c(5, 10, 15),
      nrisk = c(100, 80, 60),
      event = c(10, 15, 20),
      mcc = c(0.1, 0.3, 0.5)
    ),
    original_data = tibble::tibble(
      id = c(1, 2, 3),
      tstart = c(0, 0, 0),
      time = c(5, 10, 15),
      cause = c(1, 1, 1)
    ),
    method = "equation"
  )

  # Add group column
  result <- add_group_column_to_result(mock_result, "treatment", "A")

  # Check that group column was added to tibble components
  expect_true("treatment" %in% names(result$mcc_final))
  expect_equal(result$mcc_final$treatment, rep("A", 3))

  expect_true("treatment" %in% names(result$mcc_table))
  expect_equal(result$mcc_table$treatment, rep("A", 3))

  expect_true("treatment" %in% names(result$original_data))
  expect_equal(result$original_data$treatment, rep("A", 3))

  # Check that group column is the first column
  expect_equal(names(result$mcc_final)[1], "treatment")
  expect_equal(names(result$mcc_table)[1], "treatment")
  expect_equal(names(result$original_data)[1], "treatment")

  # Check that non-tibble components are unchanged
  expect_equal(result$method, "equation")
})

test_that("add_group_column_to_result() works with mcc_sci results", {
  # Create a mock result from mcc_sci
  mock_result <- list(
    mcc_final = tibble::tibble(
      time = c(5, 10, 15),
      SumCIs = c(0.1, 0.3, 0.5)
    ),
    sci_table = tibble::tibble(
      time = c(5, 10, 15),
      CI1 = c(0.1, 0.2, 0.3),
      SumCIs = c(0.1, 0.3, 0.5)
    ),
    all_cis = list(
      tibble::tibble(time = c(5, 10), ci = c(0.1, 0.2))
    ),
    mcc_base = tibble::tibble(
      time = c(5, 10, 15),
      cm = c(0.1, 0.2, 0.3),
      Deta = c(0.1, 0.1, 0.1),
      cumI = c(1, 1, 1)
    ),
    original_data = tibble::tibble(
      id = c(1, 2, 3),
      tstart = c(0, 0, 0),
      time = c(5, 10, 15),
      cause = c(1, 1, 1)
    ),
    method = "sci"
  )

  # Add group column
  result <- add_group_column_to_result(mock_result, "group", "Control")

  # Check that group column was added to tibble components
  expect_true("group" %in% names(result$mcc_final))
  expect_equal(result$mcc_final$group, rep("Control", 3))

  expect_true("group" %in% names(result$sci_table))
  expect_equal(result$sci_table$group, rep("Control", 3))

  expect_true("group" %in% names(result$mcc_base))
  expect_equal(result$mcc_base$group, rep("Control", 3))

  expect_true("group" %in% names(result$original_data))
  expect_equal(result$original_data$group, rep("Control", 3))

  # Check that all_cis (list component) is unchanged
  expect_equal(result$all_cis, mock_result$all_cis)

  # Check that method is unchanged
  expect_equal(result$method, "sci")
})

test_that("add_group_column_to_result() handles adjusted_data when present", {
  # Create a mock result with adjusted_data
  mock_result <- list(
    mcc_final = tibble::tibble(
      time = c(5, 10),
      mcc = c(0.1, 0.3)
    ),
    original_data = tibble::tibble(
      id = c(1, 1),
      tstart = c(0, 0),
      time = c(5, 5),
      cause = c(1, 2)
    ),
    adjusted_data = tibble::tibble(
      id = c(1, 1),
      tstart = c(0, 0),
      time = c(5.0, 5.000001),
      cause = c(1, 2)
    )
  )

  # Add group column
  result <- add_group_column_to_result(mock_result, "site", "Hospital_A")

  # Check that group column was added to adjusted_data
  expect_true("site" %in% names(result$adjusted_data))
  expect_equal(result$adjusted_data$site, rep("Hospital_A", 2))
  expect_equal(names(result$adjusted_data)[1], "site")
})

test_that("add_group_column_to_result() skips non-tibble components", {
  # Create a mock result with mixed component types
  mock_result <- list(
    mcc_final = tibble::tibble(time = 5, mcc = 0.1),
    method = "equation",
    some_vector = c(1, 2, 3),
    some_list = list(a = 1, b = 2),
    some_null = NULL
  )

  # Add group column
  result <- add_group_column_to_result(mock_result, "treatment", "B")

  # Check that only tibble components got the group column
  expect_true("treatment" %in% names(result$mcc_final))
  expect_equal(result$method, "equation")
  expect_equal(result$some_vector, c(1, 2, 3))
  expect_equal(result$some_list, list(a = 1, b = 2))
  expect_null(result$some_null)
})

test_that("add_group_column_to_result() works with different group value types", {
  mock_result <- list(
    mcc_final = tibble::tibble(time = 5, mcc = 0.1)
  )

  # Test with numeric group value
  result_numeric <- add_group_column_to_result(mock_result, "group_num", 1)
  expect_equal(result_numeric$mcc_final$group_num, 1)

  # Test with factor group value
  result_factor <- add_group_column_to_result(
    mock_result,
    "group_fct",
    factor("Level1")
  )
  expect_equal(result_factor$mcc_final$group_fct, factor("Level1"))

  # Test with logical group value
  result_logical <- add_group_column_to_result(mock_result, "group_lgl", TRUE)
  expect_equal(result_logical$mcc_final$group_lgl, TRUE)
})

test_that("combine_group_results() works with include_details = TRUE", {
  # Create mock results from two groups
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        treatment = rep("A", 2),
        time = c(5, 10),
        mcc = c(0.1, 0.3)
      ),
      mcc_table = tibble::tibble(
        treatment = rep("A", 2),
        time = c(5, 10),
        nrisk = c(50, 30),
        mcc = c(0.1, 0.3)
      ),
      original_data = tibble::tibble(
        treatment = rep("A", 2),
        id = c(1, 2),
        time = c(5, 10),
        cause = c(1, 1)
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        treatment = rep("B", 2),
        time = c(8, 12),
        mcc = c(0.2, 0.4)
      ),
      mcc_table = tibble::tibble(
        treatment = rep("B", 2),
        time = c(8, 12),
        nrisk = c(40, 25),
        mcc = c(0.2, 0.4)
      ),
      original_data = tibble::tibble(
        treatment = rep("B", 2),
        id = c(3, 4),
        time = c(8, 12),
        cause = c(1, 1)
      )
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "treatment",
    include_details = TRUE
  )

  # Check that all components are combined
  expect_true("mcc_final" %in% names(combined))
  expect_true("mcc_table" %in% names(combined))
  expect_true("original_data" %in% names(combined))

  # Check dimensions
  expect_equal(nrow(combined$mcc_final), 4) # 2 from each group
  expect_equal(nrow(combined$mcc_table), 4)
  expect_equal(nrow(combined$original_data), 4)

  # Check that both groups are represented
  expect_true(all(c("A", "B") %in% combined$mcc_final$treatment))
  expect_true(all(c("A", "B") %in% combined$mcc_table$treatment))
  expect_true(all(c("A", "B") %in% combined$original_data$treatment))

  # Check specific values
  expect_equal(combined$mcc_final$mcc, c(0.1, 0.3, 0.2, 0.4))
  expect_equal(combined$original_data$id, c(1, 2, 3, 4))
})

test_that("combine_group_results() works with include_details = FALSE", {
  # Create mock results from two groups (simplified)
  group_results <- list(
    "Control" = list(
      mcc_final = tibble::tibble(
        group = rep("Control", 2),
        time = c(5, 10),
        SumCIs = c(0.1, 0.3)
      )
    ),
    "Treatment" = list(
      mcc_final = tibble::tibble(
        group = rep("Treatment", 2),
        time = c(8, 12),
        SumCIs = c(0.2, 0.4)
      )
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "group",
    include_details = FALSE
  )

  # Check that only mcc_final is combined
  expect_true("mcc_final" %in% names(combined))
  expect_false("mcc_table" %in% names(combined))
  expect_false("original_data" %in% names(combined))

  # Check dimensions
  expect_equal(nrow(combined$mcc_final), 4)

  # Check that both groups are represented
  expect_true(all(c("Control", "Treatment") %in% combined$mcc_final$group))
  expect_equal(combined$mcc_final$SumCIs, c(0.1, 0.3, 0.2, 0.4))
})

test_that("combine_group_results() handles all_cis correctly", {
  # Create mock SCI results with all_cis
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        treatment = rep("A", 1),
        time = 5,
        SumCIs = 0.1
      ),
      all_cis = list(
        tibble::tibble(time = c(3, 5), ci = c(0.05, 0.1)),
        tibble::tibble(time = c(5), ci = c(0.02))
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        treatment = rep("B", 1),
        time = 8,
        SumCIs = 0.2
      ),
      all_cis = list(
        tibble::tibble(time = c(6, 8), ci = c(0.1, 0.15)),
        tibble::tibble(time = c(8), ci = c(0.05))
      )
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "treatment",
    include_details = TRUE
  )

  # Check that all_cis has the correct structure
  expect_true("all_cis" %in% names(combined))
  expect_true("A" %in% names(combined$all_cis))
  expect_true("B" %in% names(combined$all_cis))

  # Check that group columns were added to all_cis tibbles
  expect_true("treatment" %in% names(combined$all_cis$A[[1]]))
  expect_true("treatment" %in% names(combined$all_cis$B[[1]]))

  # Check group values
  expect_equal(unique(combined$all_cis$A[[1]]$treatment), "A")
  expect_equal(unique(combined$all_cis$B[[1]]$treatment), "B")
})

test_that("combine_group_results() handles missing components gracefully", {
  # Create mock results where not all groups have all components
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        group = "A",
        time = 5,
        mcc = 0.1
      ),
      mcc_table = tibble::tibble(
        group = "A",
        time = 5,
        mcc = 0.1
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        group = "B",
        time = 8,
        mcc = 0.2
      )
      # Note: mcc_table missing for group B
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "group",
    include_details = TRUE
  )

  # Check that mcc_final is combined (present in both)
  expect_true("mcc_final" %in% names(combined))
  expect_equal(nrow(combined$mcc_final), 2)

  # Check that mcc_table is combined (only from group A)
  expect_true("mcc_table" %in% names(combined))
  expect_equal(nrow(combined$mcc_table), 1)
  expect_equal(combined$mcc_table$group, "A")
})

test_that("combine_group_results() handles empty all_cis tibbles", {
  # Create mock results with empty all_cis tibbles
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        group = "A",
        time = 5,
        SumCIs = 0
      ),
      all_cis = list(
        tibble::tibble(time = numeric(0), ci = numeric(0)) # Empty tibble
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        group = "B",
        time = 8,
        SumCIs = 0.1
      ),
      all_cis = list(
        tibble::tibble(time = 8, ci = 0.1) # Non-empty tibble
      )
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "group",
    include_details = TRUE
  )

  # Check that all_cis structure is preserved
  expect_true("all_cis" %in% names(combined))
  expect_length(combined$all_cis, 2)

  # Check that empty tibble remains empty (no group column added)
  expect_equal(nrow(combined$all_cis$A[[1]]), 0)

  # Check that non-empty tibble has group column
  expect_true("group" %in% names(combined$all_cis$B[[1]]))
  expect_equal(combined$all_cis$B[[1]]$group, "B")
})

test_that("combine_group_results() preserves row order within groups", {
  # Create mock results with specific ordering
  group_results <- list(
    "Early" = list(
      mcc_final = tibble::tibble(
        stage = rep("Early", 3),
        time = c(1, 3, 5),
        mcc = c(0.1, 0.2, 0.3)
      )
    ),
    "Late" = list(
      mcc_final = tibble::tibble(
        stage = rep("Late", 2),
        time = c(7, 9),
        mcc = c(0.4, 0.5)
      )
    )
  )

  # Combine results
  combined <- combine_group_results(
    group_results,
    "stage",
    include_details = FALSE
  )

  # Check that ordering is preserved within groups
  early_rows <- combined$mcc_final[combined$mcc_final$stage == "Early", ]
  late_rows <- combined$mcc_final[combined$mcc_final$stage == "Late", ]

  expect_equal(early_rows$time, c(1, 3, 5))
  expect_equal(early_rows$mcc, c(0.1, 0.2, 0.3))
  expect_equal(late_rows$time, c(7, 9))
  expect_equal(late_rows$mcc, c(0.4, 0.5))
})

test_that("add_group_column_to_result() output format snapshot", {
  # Create a comprehensive mock result
  mock_result <- list(
    mcc_final = tibble::tibble(
      time = c(5, 10, 15),
      mcc = c(0.1, 0.3, 0.5)
    ),
    mcc_table = tibble::tibble(
      time = c(5, 10, 15),
      nrisk = c(100, 80, 60),
      event = c(10, 15, 20),
      censor = c(5, 10, 15),
      cmprk = c(2, 3, 4),
      overall_surv_previous = c(1.0, 0.95, 0.90),
      ave_events = c(0.1, 0.2, 0.2),
      mcc = c(0.1, 0.3, 0.5)
    ),
    original_data = tibble::tibble(
      id = c(1, 2, 3),
      tstart = c(0, 0, 0),
      time = c(5, 10, 15),
      cause = c(1, 1, 1)
    ),
    method = "equation"
  )

  expect_snapshot({
    result <- add_group_column_to_result(
      mock_result,
      "treatment_group",
      "Active"
    )

    cat("Structure after adding group column:\n")
    print(str(result, max.level = 2))

    cat("\nMCC Final:\n")
    print(result$mcc_final)

    cat("\nMCC Table (first 5 columns):\n")
    print(result$mcc_table[, 1:5])

    cat("\nOriginal Data:\n")
    print(result$original_data)
  })
})

test_that("combine_group_results() output format snapshot", {
  # Create comprehensive mock results from multiple groups
  group_results <- list(
    "Control" = list(
      mcc_final = tibble::tibble(
        treatment = rep("Control", 3),
        time = c(5, 10, 15),
        mcc = c(0.1, 0.25, 0.4)
      ),
      mcc_table = tibble::tibble(
        treatment = rep("Control", 3),
        time = c(5, 10, 15),
        nrisk = c(50, 40, 30),
        event = c(5, 8, 10),
        mcc = c(0.1, 0.25, 0.4)
      ),
      original_data = tibble::tibble(
        treatment = rep("Control", 3),
        id = c(1, 2, 3),
        tstart = c(0, 0, 0),
        time = c(5, 10, 15),
        cause = c(1, 1, 1)
      )
    ),
    "Treatment" = list(
      mcc_final = tibble::tibble(
        treatment = rep("Treatment", 2),
        time = c(8, 12),
        mcc = c(0.15, 0.35)
      ),
      mcc_table = tibble::tibble(
        treatment = rep("Treatment", 2),
        time = c(8, 12),
        nrisk = c(45, 35),
        event = c(7, 9),
        mcc = c(0.15, 0.35)
      ),
      original_data = tibble::tibble(
        treatment = rep("Treatment", 2),
        id = c(4, 5),
        tstart = c(0, 0),
        time = c(8, 12),
        cause = c(1, 1)
      )
    )
  )

  expect_snapshot({
    combined_detailed <- combine_group_results(
      group_results,
      "treatment",
      include_details = TRUE
    )

    cat("Combined results with include_details = TRUE:\n")
    cat("Components:", names(combined_detailed), "\n\n")

    cat("MCC Final:\n")
    print(combined_detailed$mcc_final)

    cat("\nMCC Table:\n")
    print(combined_detailed$mcc_table)

    cat("\nOriginal Data:\n")
    print(combined_detailed$original_data)
  })

  expect_snapshot({
    combined_simple <- combine_group_results(
      group_results,
      "treatment",
      include_details = FALSE
    )

    cat("Combined results with include_details = FALSE:\n")
    cat("Components:", names(combined_simple), "\n\n")

    cat("MCC Final:\n")
    print(combined_simple$mcc_final)
  })
})

test_that("combine_group_results() with SCI method snapshot", {
  # Create mock SCI results
  group_results <- list(
    "A" = list(
      mcc_final = tibble::tibble(
        group = rep("A", 2),
        time = c(5, 10),
        SumCIs = c(0.1, 0.3)
      ),
      sci_table = tibble::tibble(
        group = rep("A", 2),
        time = c(5, 10),
        CI1 = c(0.08, 0.2),
        CI2 = c(0.02, 0.1),
        SumCIs = c(0.1, 0.3)
      ),
      all_cis = list(
        tibble::tibble(time = c(5, 10), ci = c(0.08, 0.2)),
        tibble::tibble(time = c(10), ci = c(0.02))
      ),
      mcc_base = tibble::tibble(
        group = rep("A", 2),
        time = c(5, 10),
        cm = c(0.08, 0.2),
        Deta = c(0.08, 0.12),
        cumI = c(1, 1)
      )
    ),
    "B" = list(
      mcc_final = tibble::tibble(
        group = rep("B", 1),
        time = 8,
        SumCIs = 0.15
      ),
      sci_table = tibble::tibble(
        group = rep("B", 1),
        time = 8,
        CI1 = 0.15,
        SumCIs = 0.15
      ),
      all_cis = list(
        tibble::tibble(time = 8, ci = 0.15)
      ),
      mcc_base = tibble::tibble(
        group = rep("B", 1),
        time = 8,
        cm = 0.15,
        Deta = 0.15,
        cumI = 1
      )
    )
  )

  combined_sci <- combine_group_results(
    group_results,
    "group",
    include_details = TRUE
  )

  expect_snapshot({
    cat("SCI Combined results:\n")
    cat("Components:", names(combined_sci), "\n\n")

    cat("MCC Final:\n")
    print(combined_sci$mcc_final)

    cat("\nSCI Table:\n")
    print(combined_sci$sci_table)

    cat("\nMCC Base:\n")
    print(combined_sci$mcc_base)

    cat("\nAll CIs structure:\n")
    cat("Groups in all_cis:", names(combined_sci$all_cis), "\n")
    cat("Number of CI lists per group:\n")
    for (group_name in names(combined_sci$all_cis)) {
      cat(paste0(
        "  ",
        group_name,
        ": ",
        length(combined_sci$all_cis[[group_name]]),
        " CI tables\n"
      ))
    }

    cat("\nFirst CI table for group A:\n")
    print(combined_sci$all_cis$A[[1]])
  })
})
