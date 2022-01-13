# ui_g_scatterplot ----

test_that("ui_g_scatterplot creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_scatterplot(
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

# tm_g_scatterplot ----

test_that("tm_g_scatterplot works as expected in the sample app", {
  skip_if_too_deep(5)
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("scatterplot"),
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

  plot_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(
    plot_message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$setValue(ns("experiment-name"), "hd2")

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "cpm")

  initial_x_var <- app$waitForValue(ns("x_spec-genes"), ignore = "")
  expect_identical(initial_x_var, NULL)

  initial_y_var <- app$waitForValue(ns("y_spec-genes"), ignore = "")
  expect_identical(initial_y_var, NULL)

  # Initially there is no plot.
  plot_message <- app$waitForOutputElement(ns("plot"), "message")
  expect_identical(
    plot_message,
    "please select at least one gene"
  )

  # Set one gene each.
  app$setValue(ns("x_spec-genes"), "GeneID:5205")
  app$setValue(ns("y_spec-genes"), "GeneID:1820")

  # Change the sample filter and confirm that genes are not updated.
  ns2 <- NS("teal-main_ui-filter_panel-add_MAE_filter")
  app$setValue(ns2("hd2-col_to_add"), "ARM")

  now_x_var <- app$waitForValue(ns("x_spec-genes"))
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue(ns("y_spec-genes"))
  expect_identical(now_y_var, "GeneID:1820")

  # Change the genes filter and confirm that genes are staying the same.
  app$setValue(ns2("hd2-row_to_add"), "chromosome")

  now_x_var <- app$waitForValue(ns("x_spec-genes"))
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue(ns("y_spec-genes"))
  expect_identical(now_y_var, "GeneID:1820")

  # Now change the experiment_name, genes, method.
  app$setValue(ns("experiment-name"), "hd2")
  app$setValue(ns("x_spec-genes"), "GeneID:5205")
  app$setValue(ns("y_spec-genes"), "GeneID:102723793")
  app$setValue(ns("smooth_method"), "loess")
  app$setValue(ns("facet_var-sample_var"), "AGE18")

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = ns("plot"),
    name = "final_plot.png"
  )
})
