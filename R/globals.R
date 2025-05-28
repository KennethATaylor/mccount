utils::globalVariables(c(
  # Data manipulation variables
  "id",
  "time",
  "cause",
  "tstart",

  # data.table variables
  "n_events_at_time",
  ".N",
  ".",
  ".SD",
  "cause_priority",
  "row_within_time",

  # MCC calculation variables
  "first",
  "maxE",
  "i.maxE",
  "m_event",
  "time_adj",
  "tstart_adj",
  "cause1",
  "Time",
  "cm",
  "Deta",
  "cumI",
  "MCC",
  "SumCIs",

  # Lifetable variables
  "count",
  "censor",
  "event",
  "cmprk",
  "sum_censor",
  "sum_cmprk",
  "nrisk_current",
  "nrisk",
  "surv_prob",
  "overall_surv",
  "overall_surv_previous",
  "ave_events",
  "mcc",

  # General variables
  "V1",
  "ci"
))
