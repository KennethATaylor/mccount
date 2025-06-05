# Set consistent options
withr::local_options(
  list(
    width = 20
  ),
  .local_envir = teardown_env()
)
