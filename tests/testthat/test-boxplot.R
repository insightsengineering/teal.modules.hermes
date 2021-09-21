# ui_g_boxplot ----

test_that("ui_g_boxplot creates expected HTML", {
  mae_name <- "MyMAE"
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_boxplot(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_boxplot ----

test_that("tm_g_boxplot works as expected in the sample app", {
  test.nest::skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(
    "boxplot/",
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
  # label: boxplot-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-assay_name")
  expect_identical(initial_assay_name, "counts")

  initial_x_var <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-x_var", ignore = "")
  expect_identical(initial_x_var, NULL)

  initial_genes <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-genes")
  expect_identical(initial_genes, "GeneID:101927746")

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_boxplot-plot",
    name = "initial_plot.png"
  )

  # Now change the experiment_name and confirm that the gene is updated accordingly.
  app$setInputs(
    "teal-main_ui-modules_ui-root_boxplot-experiment_name" = "hd3"
  )
  now_genes <- app$waitForValue("teal-main_ui-modules_ui-root_boxplot-genes")
  expect_identical(now_genes, "GeneID:5205")

  # Update boxplot with Jitter, select multiple genes and x variable and update to violin plot.
  app$setInputs(
    "teal-main_ui-modules_ui-root_boxplot-jitter" = TRUE,
    "teal-main_ui-modules_ui-root_boxplot-violin" = TRUE,
    "teal-main_ui-modules_ui-root_boxplot-genes" = c("GeneID:5205"),
    "teal-main_ui-modules_ui-root_boxplot-x_var" = "COUNTRY",
    "teal-main_ui-modules_ui-root_boxplot-color_var" = "AGE18"
  )

  # Final plot.
  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_boxplot-plot",
    name = "final_plot.png"
  )
  app$stop()
})
