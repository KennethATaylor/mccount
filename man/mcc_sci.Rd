% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcc_sci.R
\name{mcc_sci}
\alias{mcc_sci}
\title{Calculate Mean Cumulative Count using the Sum of Cumulative Incidence method}
\usage{
mcc_sci(
  data,
  id_var,
  time_var,
  cause_var,
  tstart_var = NULL,
  adjust_times = TRUE,
  time_precision = 1e-06,
  include_details = TRUE
)
}
\arguments{
\item{data}{A data.frame or tibble containing the required variables}

\item{id_var}{Name of the column containing participant IDs (as string or symbol)}

\item{time_var}{Name of the column containing follow-up or event times (as string or symbol)}

\item{cause_var}{Name of the column containing event indicators (as string or symbol)
(1=event of interest, 2=competing risk, 0=censoring)}

\item{tstart_var}{Name of the column containing start times of follow-up (as string or symbol, optional).
If NULL (default), a constant value of 0 is used for all observations.}

\item{adjust_times}{Whether to automatically adjust times for simultaneous events (default: TRUE)}

\item{time_precision}{Precision used for adjusting simultaneous events (default: 1e-6)}

\item{include_details}{Whether to include detailed calculation tables and intermediate
objects in the output (default: TRUE).}
}
\value{
A list containing MCC results. If include_details=TRUE, returns complete
calculation details. Otherwise, returns only the final MCC estimates.
}
\description{
Calculate Mean Cumulative Count using the Sum of Cumulative Incidence method
}
\keyword{internal}
