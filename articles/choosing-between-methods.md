# Choosing Between Methods

## Two Approaches, Same Goal

`mccount` implements two methods for calculating mean cumulative count
(MCC):

- **Equation method**: Direct calculation using the Dong-Yasui estimator
- **SCI method**: Sum of cumulative incidences approach

Both estimate the same parameter and are **mathematically equivalent**
under specific conditions. Under conditions where the two estimators
will differ, the choice between the two estimators should be made based
on the research question at hand, specifically the characteristics of
the outcome.

## When Methods Are Equivalent

Both MCC estimators yield **identical results** under specific
conditions:

### Administrative Censoring Only

The methods are mathematically equivalent when there is no censoring
except for at the end of study follow-up (i.e. *administrative censoring
only*) - that is, when you observe all participants until they either
experience a competing risk or reach the end of study follow-up.

### Synchronized Event-Interval Censoring

Even with censoring before the end of follow-up, the two methods yield
identical results when censoring follows a very specific pattern.
Censoring must occur after *everyone* remaining at risk has experienced
their *p*th event, but before *anyone* has had a (*p*+1)th. This
censoring pattern is observed when non-administrative censoring is
*only* observed during time intervals where the event count sequence is
synchronized at the cohort level. I like to call this pattern
*synchronized event-interval censoring*.

Take the applied example given by Dong, *et al*.[¹](#fn1) - assume 5
participants enrolled in hypothetical study:

- Subject 1: alive at the end of follow-up and administratively censored
  at that time (`t8`)
- Subject 2: Lost to follow-up and censored at `t1`
- Subject 3: Died (competing-risk event) at `t5`
- Subject 4: Experience the event of interest at `t2`, `t6`, and `t7`
  and was alive at the end of follow-up (administratively censored at
  `t8`)
- Subject 5: Experienced event of interest once at `t3` and died
  (competing risk event) at the same time point

``` r
df <- data.frame(
  id = c(1, 2, 3, 4, 4, 4, 4, 5, 5),
  time = c(8, 1, 5, 2, 6, 7, 8, 3, 3),
  cause = c(0, 0, 2, 1, 1, 1, 0, 1, 2)
 )

# Calculating MCC using both estimators
mcc_eq <- mcc(
  df,
  id_var = "id",
  time_var = "time",
  cause_var = "cause",
  method = "equation"
)
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.

mcc_sci <- mcc(
  df,
  id_var = "id",
  time_var = "time",
  cause_var = "cause",
  method = "sci"
)
#> ℹ Adjusted time points for events occurring simultaneously for the same subject.
```

In this example, both methods are equivalent because the only
non-administrative censoring that happens is before *everyone’s* first
event (i.e., synchronized event-interval censoring):

``` r
# Dong-Yasui estimator
mcc_details(mcc_eq)
#> # A tibble: 9 × 8
#>    time nrisk censor event cmprk overall_surv_previous ave_events   mcc
#>   <dbl> <dbl>  <dbl> <dbl> <dbl>                 <dbl>      <dbl> <dbl>
#> 1  0        5      0     0     0                  1          0     0   
#> 2  1        5      1     0     0                  1          0     0   
#> 3  2        4      0     1     0                  1          0.25  0.25
#> 4  3        4      0     1     0                  1          0.25  0.5 
#> 5  3.00     4      0     0     1                  1          0     0.5 
#> 6  5        3      0     0     1                  0.75       0     0.5 
#> 7  6        2      0     1     0                  0.5        0.25  0.75
#> 8  7        2      0     1     0                  0.5        0.25  1   
#> 9  8        2      2     0     0                  0.5        0     1

# Sum of cumulative incidences estimator
mcc_details(mcc_sci)
#> # A tibble: 9 × 5
#>    time   CI1   CI2   CI3 SumCIs
#>   <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1  0     0     0     0      0   
#> 2  1     0     0     0      0   
#> 3  2     0.25  0     0      0.25
#> 4  3     0.5   0     0      0.5 
#> 5  3.00  0.5   0     0      0.5 
#> 6  5     0.5   0     0      0.5 
#> 7  6     0.5   0.25  0      0.75
#> 8  7     0.5   0.25  0.25   1   
#> 9  8     0.5   0.25  0.25   1
```

## When Methods Differ and Why

When results from the two estimators differ, it reflects how they handle
a fundamental question about the outcome: **Does event order matter for
your outcome?** The Dong-Yasui estimator treats all events as
exchangeable - when someone is censored after their 2nd event, it
affects the calculation of *all* future events regardless of whether
they are the 1st, 2nd, or 3rd for other people. The SCI method takes an
event-specific approach - when someone is censored after their 2nd
event, it only affects calculations for 3rd+ events, not 1st/2nd events
for others.

### When Events Are Exchangeable

Recurrent events in a healthcare context might be reasonably considered
exchangeable if their ordering is irrelevant clinically or biologically
because all events represent the same underlying process or a routine,
maintenance-type activities where clinical meaning is unrelated to the
order of the recurrent event count. Consider the following examples:

- **Routine medication refills**: Each refill of chronic medications
  (blood pressure pills, diabetes medications) might represent identical
  adherence behavior
- **Preventive care visits**: Annual physicals or routine dental
  cleanings represent the same preventive behavior whether it’s the 3rd
  or 10th visit
- **Routine lab monitoring**: Quarterly hemoglobin A1c tests for stable
  diabetes may represent identical surveillance activity
- **Routine imaging surveillance**: Follow-up mammograms or CT scans for
  stable conditions represent the same monitoring process

### When Event Order Matters

Exchangeability is unreasonable (or naive) for some recurrent outcomes
in health research. With some outcomes, the occurrence of each event may
impact the probability of subsequent events (i.e. the events have
different clinical meaning or biological mechanisms depending on where
they fall in the count sequence). Consider the following examples:

- **Cancer recurrences**: First vs second recurrence may have different
  biology and prognosis
- **Hospital readmissions**: Early readmissions may indicate discharge
  planning issues, while multiple readmissions suggest complex care
  needs
- **Medication adverse events**: Early vs late adverse events may have
  different mechanisms and clinical significance
- **Seizures**: Early vs late seizures may indicate different disease
  progression patterns (depending on the underlying cause)

## Documenting Your Choice

If you are in the position where the censoring pattern will result in
equivalent results from the two estimators, then you can make the
decision of which MCC estimator to use based on computational efficiency
and pick the Dong-Yasui estimator. For the rest of us who are dealing
with scenarios where the estimators are expected to differ, the choice
between estimators should align with the outcome of interest in the
research question. Regardless of which group you fall into, I recommend
explicitly documenting which estimator was used (and how the choice was
made).

## Special Case - Delayed Study Entry

If patients enter your study at different times (i.e., left-truncated
follow-up time), only the SCI method supports this through the
`tstart_var` parameter. See the
[`mcc()`](https://kennethataylor.github.io/mccount/reference/mcc.md)
documentation for more details.

## Summary

1.  **Both methods estimate the same parameter** and are mathematically
    equivalent under certain conditions
2.  **Choose based on your research question**:
    - Events exchangeable → Equation method
    - Event order matters → SCI method
3.  **Special requirements**: Use SCI method if you need left truncation
    support

------------------------------------------------------------------------

1.  Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui Y.
    Estimating the burden of recurrent events in the presence of
    competing risks: the method of mean cumulative count. *Am J
    Epidemiol*. 2015 Apr 1;181(7):532-40. doi:
    [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)
