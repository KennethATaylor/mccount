# Set consistent options
withr::local_options(
  list(
    width = 20,
    cli.unicode = FALSE,
    cli.ansi = FALSE,
    crayon.enabled = FALSE,
    pillar.subtle = FALSE,
    pillar.emphasis = FALSE
  ),
  .local_envir = teardown_env()
)
