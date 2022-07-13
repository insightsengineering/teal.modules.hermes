# ui_g_barplot ----

test_that("ui_g_barplot creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(999)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_barplot(
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

# tm_g_barplot ----

test_that("tm_g_barplot works as expected in the sample app", {
  skip_if_too_deep(5)
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("barplot"), loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  ns <- module_ns(app)

  # Check initial experiment name.
  initial_experiment_name <- app$waitForValue(ns("experiment-name"))
  expect_identical(initial_experiment_name, "hd1")

  # Check initial assay name
  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "counts")

  # Check that no gene is initially selected.
  initial_x_spec <- app$waitForValue(ns("x-genes"), ignore = "")
  expect_identical(initial_x_spec, NULL)

  # Check that a message initially replaces the plot.
  plot_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(
    plot_message,
    "please select at least one gene"
  )

  # Set gene value
  app$setValue(ns("experiment-name"), "hd2")
  app$setValue(ns("assay-name"), "tpm")
  app$setValue(ns("x-genes"), "GeneID:8086")
  app$setValue(ns("experiment-name"), "hd1")

  # Check that gene list is updated
  now_x_spec_gene <- app$waitForValue(ns("x-genes"), ignore = "")
  expect_identical(now_x_spec_gene, "GeneID:8086")

  # Check that assay list is updated
  now_assay <- app$waitForValue(ns("assay"), ignore = "")
  expect_null(now_assay)

  # Check error message in case of identical percentile boundaries
  app$setValue(ns("percentiles"), c(0.1, 0.1))
  plot_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(
    plot_message,
    "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
  )

  # Set Experiment, assay, gene, percentile and facet
  app$setValue(ns("experiment-name"), "hd1")
  app$setValue(ns("assay-name"), "counts")
  app$setValue(ns("x-genes"), "GeneID:47")
  app$setValue(ns("percentiles"), c(0.2, 0.8))
  app$setValue(ns("facet-sample_var"), "AGE18")

  expect_snapshot_screenshot(
    app,
    id = ns("plot"),
    name = "final_plot.png"
  )
})
