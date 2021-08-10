# ui_g_quality ----

test_that("ui_g_quality creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_quality(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_quality ----

test_that("tm_g_quality works as expected in the sample app", {
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(
    "quality/",
    loadTimeout = 1e5,
    debug = "all",
    phantomTimeout = 1e5,
    seed = 123
  )
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: quality-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_quality-experiment_name")
  expect_identical(initial_experiment_name, "se1")

  # initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_quality-assay_name")
  # expect_identical(initial_assay_name, "counts")

  # initial_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-x_var")
  # expect_identical(initial_x_var, "Filename")
  #
  # initial_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-y_var")
  # expect_identical(initial_y_var, "GeneID:101927746")

  # # Initial plot.
  # expect_snapshot_screenshot(
  #   app,
  #   id = "teal-main_ui-modules_ui-root_boxplot-plot",
  #   name = "initial_plot.png"
  # )
  #
  # # Now change the experiment_name and confirm that the gene and stratifyin variables are updated accordingly.
  # app$setInputs(
  #   "teal-main_ui-modules_ui-root_boxplot-experiment_name" = "se3"
  # )
  # now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-x_var")
  # expect_identical(now_x_var, "Filename")
  #
  # now_y_var <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-y_var")
  # expect_identical(now_y_var, "GeneID:5205")
  #
  # # Also now the plot exists.
  # final_output <- app$getOutputValue("teal-main_ui-modules_ui-root_boxplot-plot")
  # expect_identical(final_output$alt, "Plot object")
  # expect_match(final_output$src, "^data:image/png")
  #
  # # Final plot.
  # expect_snapshot_screenshot(
  #   app,
  #   id = "teal-main_ui-modules_ui-root_boxplot-plot",
  #   name = "final_plot.png"
  # )
  #
  app$stop()
})
