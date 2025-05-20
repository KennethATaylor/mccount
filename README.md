
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mccount <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mccount)](https://CRAN.R-project.org/package=mccount)

<!-- badges: end -->

## Overview

`mccount` provides tools to estimate the mean cumulative count (MCC) of
recurrent events in the presence of competing risks. Unlike traditional
cumulative incidence methods that only consider the first occurrence of
an event, MCC accounts for multiple occurrences of the same event type
per individual, providing a more informative measure of the total burden
of recurrent events in a population.

`mccount` provides an interface to estimate the MCC using either the
Dong-Yasui estimator of the MCC or the sum of cumulative incidences,
based on the work of Dong, *et al*.[^1]

## Installation

You can install the development version of `mccount` like so:

``` r
# install.packages("pak")
pak::pkg_install("KennethATaylor/mccount")
```

## Why Mean Cumulative Count?

In the context of clinical and epidemiological studies, subjects may
experience multiple recurrent events over a given follow-up period.
Traditional methods like Kaplan-Meier or cumulative incidence only
account for the *first* occurrence of an event and ignore subsequent
recurrent events, which can lead to underestimation of the true disease
burden.

The MCC:

- Summarizes all events that occur in the population by a given time
- Accounts for competing risks that may terminate follow-up
- Provides a more complete picture of the recurrent event burden
- Is interpretable as the expected number of events per subject by a
  specific time point

## Citation

If you use `mccount` in your research, please cite the package in
addition to the original methodology paper.[^2]

``` r
citation("mccount")
```

[^1]: Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui
    Y. Estimating the burden of recurrent events in the presence of
    competing risks: the method of mean cumulative count. *Am J
    Epidemiol*. 2015 Apr 1;181(7):532-40. doi:
    [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)

[^2]: Dong H, Robison LL, Leisenring WM, Martin LJ, Armstrong GT, Yasui
    Y. Estimating the burden of recurrent events in the presence of
    competing risks: the method of mean cumulative count. *Am J
    Epidemiol*. 2015 Apr 1;181(7):532-40. doi:
    [10.1093/aje/kwu289](https://doi.org/10.1093/aje/kwu289)
