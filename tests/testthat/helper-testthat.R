# Shortcut to skip a shinytest when coverage is running.
skip_if_covr <- function() {
  covr_is_running <- covr::in_covr()
  skip_if(
    covr_is_running,
    message = "not running shinytest while covr is running"
  )
}

# Adapted from proposal by Hadley (https://github.com/rstudio/shinytest/issues/353).
expect_snapshot_screenshot <- function(app,
                                       id = NULL,
                                       name = "screenshot.png",
                                       parent = FALSE,
                                       wait_for_plot = FALSE) {
  testthat::skip_on_ci()

  assert_flag(wait_for_plot)

  path <- tempfile()
  app$waitForShiny()
  if (wait_for_plot) {
    app$waitForOutputElement(id, "alt", timeout = 1e5)
  }
  Sys.sleep(0.5)
  app$takeScreenshot(path, id, parent = parent)
  testthat::expect_snapshot_file(path, name)
}
