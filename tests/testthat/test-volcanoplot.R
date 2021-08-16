# ui_g_volcanoplot ----

test_that("ui_g_volcanoplot creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_volcanoplot(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_volcanoplot ----

test_that("tm_g_volcanoplot works as expected in the sample app", {
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("volcanoplot/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: volcanoplot-
  # inputId: experiment_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_volcanoplot-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  initial_compare_group <- app$waitForValue("teal-main_ui-modules_ui-root_volcanoplot-compare_group")
  expect_identical(initial_compare_group, "AGE18")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_volcanoplot-plot",
    name = "initial_plot.png"
  )

  # Now change the log2_fc_thresh and check that the plot is updated accordingly.
  app$setInputs(
    "teal-main_ui-modules_ui-root_volcanoplot-log2_fc_thresh" = 8
  )

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_volcanoplot-plot",
    name = "final_plot.png"
  )

  app$stop()
})
