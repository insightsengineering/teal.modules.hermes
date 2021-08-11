# ui_g_barplot ----

test_that("ui_g_barplot creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_barplot(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_barplot ----

test_that("tm_g_barplot works as expected in the sample app", {
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("barplot/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  # Note: left hand side name is composed as:
  # prefix: teal-main_ui-modules_ui-root_
  # label: barplot-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_barplot-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_barplot-assay_name")
  expect_identical(initial_assay_name, "counts")

  initial_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_barplot-x_var")
  expect_identical(initial_x_var, "GeneID:101927746")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_barplot-plot",
    name = "initial_plot.png"
  )

  # Now change the experiment_name and confirm that genes are updated accordingly.
  app$setInputs(
    "teal-main_ui-modules_ui-root_barplot-experiment_name" = "hd3"
  )
  now_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_barplot-x_var")
  expect_identical(now_x_var, "GeneID:5205")

  # Test that validation message is correct when select two equal quantiles with the slider.
  app$setInputs(
    "teal-main_ui-modules_ui-root_barplot-percentiles" = c(0.2, 0.2)
  )
  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_barplot-plot", "message")
  expect_match(plot_message, "please select two different quantiles")

  # Change percentiles to something that works.
  app$setInputs(
    "teal-main_ui-modules_ui-root_barplot-percentiles" = c(0.2, 0.5)
  )

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_barplot-plot",
    name = "final_plot.png"
  )

  app$stop()
})
