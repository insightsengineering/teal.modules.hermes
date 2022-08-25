# ui_g_boxplot ----

test_that("ui_g_boxplot creates expected HTML", {
  mae_name <- "MyMAE"
  data <- list(MyMAE = reactive(hermes::multi_assay_experiment))
  result <- ui_g_boxplot(
    id = "testid",
    data = data,
    mae_name = mae_name,
    summary_funs = list(Mean = colMeans),
    pre_output = NULL,
    post_output = NULL
  )
  expect_tag(result)
})

# tm_g_boxplot ----

test_that("tm_g_boxplot works as expected in the sample app", {
  skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("boxplot"),
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

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "counts")

  initial_strat <- app$waitForValue(ns("strat-sample_var"), ignore = "")
  expect_identical(initial_strat, NULL)

  initial_genes <- app$waitForValue(ns("genes-genes"), ignore = "")
  expect_identical(initial_genes, NULL)

  # Initial validation message.
  output_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(output_message, "please select at least one gene")

  # Do a couple of updates to obtain a plot.
  app$setValues(
    "jitter" = TRUE,
    "violin" = TRUE,
    "genes-genes" = "GeneID:5205",
    "strat-sample_var" = "COUNTRY",
    "color-sample_var" = "AGE18",
    ns = ns
  )

  expect_snapshot_screenshot(
    app,
    id = ns("plot"),
    name = "boxplot.png"
  )
})
