# ui_g_scatterplot ----

test_that("ui_g_scatterplot creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_scatterplot(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_scatterplot ----

test_that("tm_g_scatterplot works as expected in the sample app", {
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("scatterplot/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: scatterplot-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  output <- app$getOutputValue("teal-main_ui-modules_ui-root_scatterplot-plot")
  expect_identical(output$message, "no assays are available for this experiment, please choose another experiment")

  # Choose another experiment.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-experiment_name" = "hd2"
  )

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-assay_name")
  expect_identical(initial_assay_name, "cpm")

  initial_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var")
  expect_identical(initial_x_var, "GeneID:101927746")

  initial_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var")
  expect_identical(initial_y_var, "GeneID:1820")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_scatterplot-plot",
    name = "initial_plot.png"
  )

  # Check what happens if genes are the same.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-x_var" = "GeneID:5205",
    "teal-main_ui-modules_ui-root_scatterplot-y_var" = "GeneID:5205"
  )
  output <- app$getOutputValue("teal-main_ui-modules_ui-root_scatterplot-plot")
  expect_identical(output$message, "please select different genes for x and y variables")

  # Change the sample filter and confirm that genes are not updated.
  app$setInputs(
    "teal-main_ui-filter_panel-add_MAE_filter-hd2-col_to_add" = "ARM"
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var")
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var")
  expect_identical(now_y_var, "GeneID:5205")

  # Change the genes filter and confirm that genes are now updated.
  app$setInputs(
    "teal-main_ui-filter_panel-add_MAE_filter-hd2-row_to_add" = "Chromosome"
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var")
  expect_identical(now_x_var, "GeneID:101927746")

  now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var")
  expect_identical(now_y_var, "GeneID:1820")

  # Now change the experiment_name and confirm that genes are updated accordingly.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-experiment_name" = "hd3"
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-x_var")
  expect_identical(now_x_var, "GeneID:5205")

  now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_scatterplot-y_var")
  expect_identical(now_y_var, "GeneID:102723793")

  # Change back to working experiment and to Loess smoother.
  app$setInputs(
    "teal-main_ui-modules_ui-root_scatterplot-experiment_name" = "hd2",
    "teal-main_ui-modules_ui-root_scatterplot-smooth_method" = "loess"
  )

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_scatterplot-plot",
    name = "final_plot.png"
  )

  app$stop()
})
