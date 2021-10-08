# ui_g_km ----

test_that("ui_g_km creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_km(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_km ----

test_that("tm_g_km works as expected in the sample app", {
  test.nest::skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("km/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  ns <- NS("teal-main_ui-modules_ui-root_kaplan_meier")

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue(ns("experiment-name"))
  expect_identical(initial_experiment_name, "hd1")

  plot_message <- app$waitForOutputElement(ns("km_plot"), "message")
  expect_identical(
    plot_message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$setValue(ns("experiment-name"), "hd2")

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "cpm")

  # Choose a gene signature.
  app$setValue(ns("genes-genes"), c("GeneID:101927746", "GeneID:1820"))

  # Initial plot.
  Sys.sleep(1)
  expect_snapshot_screenshot(
    app,
    id = ns("km_plot"),
    name = "initial_plot.png"
  )

  app$stop()
})
