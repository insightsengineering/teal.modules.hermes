# Shortcut to skip a shinytest when coverage is running.
skip_if_covr <- function() {
  covr_is_running <- covr::in_covr()
  skip_if(
    covr_is_running,
    message = "not running shinytest while covr is running"
  )
}
