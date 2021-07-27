# Shortcut to skip a shinytest when coverage is running.
skip_if_covr <- function() {
  covr_is_running <- test.nest:::is_covr()
  skip_if(
    covr_is_running,
    message = "not running shinytest while covr is running"
  )
}

# Adapted from proposal by Hadley (https://github.com/rstudio/shinytest/issues/353).
expect_snapshot_screenshot <- function(app,
                                       id = NULL,
                                       name = "screenshot.png",
                                       parent = FALSE) {
  path <- tempfile()
  app$takeScreenshot(path, id, parent = parent)
  expect_snapshot_file(path, name)
}
