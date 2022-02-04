# ui_g_volcanoplot ----

test_that("ui_g_volcanoplot creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  result <- ui_g_volcanoplot(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  )
  expect_tag(result)
})

# tm_g_volcanoplot ----

test_that("tm_g_volcanoplot works as expected in the sample app", {
  skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("volcanoplot"),
    loadTimeout = 1e5,
    debug = "all", phantomTimeout = 1e5, seed = 123
  )
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  ns <- module_ns(app)

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue(ns("experiment-name"))
  expect_identical(initial_experiment_name, "hd1")

  initial_compare_group <- app$waitForValue(ns("compare_group-sample_var"), ignore = "")
  expect_identical(initial_compare_group, NULL)

  output_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(output_message, "Please select a group variable")

  # Select an initial group variable.
  app$setValue(ns("compare_group-sample_var"), "AGE18")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = ns("plot"),
    name = "initial_plot.png"
  )

  # Now change the log2_fc_thresh and check that the plot is updated accordingly.
  app$setValue(ns("log2_fc_thresh"), 8)

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = ns("plot"),
    name = "final_plot.png"
  )
})
