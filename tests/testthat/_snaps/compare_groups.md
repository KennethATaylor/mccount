# compare_groups rejects non-mcc objects

    Code
      compare_groups(df)
    Condition
      Error in `validate_comparison_inputs()`:
      ! `x` must be an <mcc> object

# compare_groups rejects ungrouped mcc objects

    Code
      compare_groups(mcc_ungrouped)
    Condition
      Error in `validate_comparison_inputs()`:
      ! `x` must be a grouped <mcc> object
      i Use the `by` argument in `mcc()` to create grouped analyses

---

    Code
      compare_groups(mcc_ungrouped)
    Condition
      Error in `validate_comparison_inputs()`:
      ! `x` must be a grouped <mcc> object
      i Use the `by` argument in `mcc()` to create grouped analyses

# compare_groups rejects invalid reference groups

    Code
      compare_groups(mcc_grouped, reference = "Z")
    Condition
      Error in `validate_comparison_inputs()`:
      ! Reference group "Z" not found in `x`
      i Available groups: "A" and "B"

# compare_groups rejects multiple references when pairwise = FALSE

    Code
      compare_groups(mcc_grouped, reference = c("A", "B"), pairwise = FALSE)
    Condition
      Error in `validate_comparison_inputs()`:
      ! `reference` must be a single value when `pairwise = FALSE`
      x You provided 2 values
      i To use multiple reference preferences, set `pairwise = TRUE`

---

    Code
      compare_groups(mcc_grouped, reference = c("A", "B"), pairwise = FALSE)
    Condition
      Error in `validate_comparison_inputs()`:
      ! `reference` must be a single value when `pairwise = FALSE`
      x You provided 2 values
      i To use multiple reference preferences, set `pairwise = TRUE`

# compare_groups rejects invalid reference preferences in pairwise mode

    Code
      compare_groups(mcc_grouped, reference = c("A", "Z"), pairwise = TRUE)
    Condition
      Error in `validate_comparison_inputs()`:
      ! Reference preference not found in `x`: "Z"
      i Available groups: "A", "B", and "C"

# compare_groups validates measure argument

    Code
      compare_groups(mcc_grouped, measure = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "difference", "ratio", "both"

# compare_groups works with 2 groups using default reference

    Code
      result <- compare_groups(mcc_grouped)
    Message
      i Using "A" as reference group
      i To change: specify `reference = "B"`

# compare_groups handles 3+ groups with default reference

    Code
      result <- compare_groups(mcc_grouped)
    Condition
      Warning:
      ! Multiple groups detected but no reference specified
      i Using "A" as reference group
      i To change: specify `reference` or set `pairwise = TRUE`
    Message
      i Max follow-up: C = 6, A = 8
      i Valid comparison period: time 0 to 6
      i Comparison of A and C limited to on or before time 6 and MCCD/MCCR will be NA beyond that time

# compare_groups performs all pairwise comparisons

    Code
      result <- compare_groups(mcc_grouped, pairwise = TRUE)
    Message
      i Max follow-up: C = 6, A = 8
      i Valid comparison period: time 0 to 6
      i Comparison of A and C limited to on or before time 6 and MCCD/MCCR will be NA beyond that time
      i Max follow-up: C = 6, B = 8
      i Valid comparison period: time 0 to 6
      i Comparison of B and C limited to on or before time 6 and MCCD/MCCR will be NA beyond that time
    Condition
      Warning:
      ! No `reference` group preference found for 3 comparisons
      i Using alphabetical default for: "B vs A", "C vs A", and "C vs B"
      i To avoid this, ensure `reference` preferences cover all comparisons

# compare_groups handles division by zero in MCCR

    Code
      result <- compare_groups(mcc_grouped, reference = "A", measure = "ratio")
    Message
      i Max follow-up: B = 5, A = 1
      i Valid comparison period: time 0 to 1
      i Comparison of A and B limited to on or before time 1 and MCCD/MCCR will be NA beyond that time
    Condition
      Warning:
      ! Reference group MCC = 0 at NA time points
      i MCCR set to "NA" at these times
      i Comparison: "B" vs "A"

