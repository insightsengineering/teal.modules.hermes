# ui_g_forest_tte ----

test_that("ui_g_forest_tte creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  result <- ui_g_forest_tte(
    id = "testid",
    datasets = datasets,
    adtte_name = "ADTTE",
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  )
  expect_tag(result)
})

# tm_g_forest_tte ----

test_that("tm_g_forest_tte works as expected in the sample app", {
  utils.nest::skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("forest_tte"), loadTimeout = 1e5,
                         debug = "all", phantomTimeout = 1e5, seed = 123)
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  ns <- shiny_elem_ns(app$getSource())

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue(ns("experiment-name"))
  expect_identical(initial_experiment_name, "hd1")

  plot_message <- app$waitForOutputElement(ns("plot-plot_main"), "message")
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

  # Choose an endpoint.
  msg <- app$waitForOutputElement(ns("plot-plot_main"), "message")
  expect_identical(msg, "please select an endpoint")
  app$setValue(ns("adtte-paramcd"), "PFS")

  # Initial plot.
  app$waitForShiny()
  Sys.sleep(1)
  expect_snapshot_screenshot(
    app,
    id = ns("plot-plot_out_main"),
    name = "initial_plot.png"
  )
})
