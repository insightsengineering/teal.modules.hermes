# experimentSpecInput ----

test_that("experimentSpecInput creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(experimentSpecInput(
    "my_experiment",
    datasets = datasets,
    mae_name = mae_name,
    label_experiments = "Please select the best experiment"
  ))
})

# experimentSpecServer ----

test_that("experimentSpec module works as expected in the test app", {
  skip_if_covr()
  test.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new("experimentSpec/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  ns <- NS("teal-main_ui-modules_ui-root_experimentSpec_example")

  # Initially the first experiment is selected.
  initial_experiment <- app$waitForValue(ns("my_experiment-name"))
  expect_identical(initial_experiment, "hd1")

  # The data is correctly processed so we can see the print result.

})
