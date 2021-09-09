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
  test.nest::skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("quality/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: quality-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_quality-experiment-name")
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_quality-assay_name")
  expect_identical(initial_assay_name, "counts")

  initial_filter <- app$waitForValue("teal-main_ui-modules_ui-root_quality-filter")
  expect_identical(initial_filter, c("genes", "samples"))

  initial_plot_type <- app$waitForValue("teal-main_ui-modules_ui-root_quality-plot_type")
  expect_identical(initial_plot_type, "Histogram")

  # Check that warning message for at least 2 genes works as expected.
  app$setInputs(
    "teal-main_ui-modules_ui-root_quality-min_cpm" = 54356
  )
  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_quality-plot", "message")
  expect_identical(plot_message, "Please change gene filters to ensure that there are at least 2 genes")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_quality-plot",
    name = "initial_plot.png"
  )

  # Choose another experiment.
  app$setInputs(
    "teal-main_ui-modules_ui-root_quality-experiment-name" = "hd2"
  )

  # Check that warning message about already present quality flags works as expected.
  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_quality-plot", "message")
  expect_identical(plot_message, "Quality flags have already been added to this experiment")

  # Choose another experiment.
  app$setInputs(
    "teal-main_ui-modules_ui-root_quality-experiment-name" = "hd3"
  )

  app$setInputs(
    "teal-main_ui-modules_ui-root_quality-min_depth" = "Specify"
  )

  # Check state of encodings again.
  initial_min_cpm <- app$waitForValue("teal-main_ui-modules_ui-root_quality-min_cpm")
  expect_identical(initial_min_cpm, 26L)

  initial_min_depth_continuous <- app$waitForValue("teal-main_ui-modules_ui-root_quality-min_depth_continuous")
  expect_identical(initial_min_depth_continuous, 1777260L)

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_quality-plot",
    name = "final_plot.png"
  )

  app$stop()
})
