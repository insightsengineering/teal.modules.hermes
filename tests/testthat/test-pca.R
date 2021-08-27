# ui_g_pca ----

test_that("ui_g_pca creates expected HTML", {
  mae_name <- "MyMAE"
  shiny:::withPrivateSeed(set.seed(123))
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_pca(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
})

test_that("tm_g_pca works as expected in the sample app", {
  test.nest::skip_if_too_deep(5)
  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new(
    "pca/",
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
  # label: pca-
  # inputId: assay_name

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_pca-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_pca-assay_name")
  expect_identical(initial_assay_name, "counts")

  initial_tab <- app$waitForValue("teal-main_ui-modules_ui-root_pca-tab_selected")
  expect_identical(initial_tab, "PCA")

  initial_xvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-x_var")
  expect_identical(initial_xvar, "1")

  initial_yvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-y_var")
  expect_identical(initial_yvar, "2")

  initial_varpct <- app$waitForValue("teal-main_ui-modules_ui-root_pca-var_pct")
  expect_identical(initial_varpct, TRUE)

  initial_label <- app$waitForValue("teal-main_ui-modules_ui-root_pca-label")
  expect_identical(initial_label, TRUE)

  initial_matrix <- app$waitForValue("teal-main_ui-modules_ui-root_pca-show_matrix")
  expect_identical(initial_matrix, TRUE)

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_pca",
    name = "initial_pca_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_pca",
    name = "initial_pca_table.png"
  )

  # Now update the tab selection.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PC and Sample Correlation"
  )

  initial_experiment_name <- app$waitForValue("teal-main_ui-modules_ui-root_pca-experiment_name")
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue("teal-main_ui-modules_ui-root_pca-assay_name")
  expect_identical(initial_assay_name, "counts")

  initial_cluster <- app$waitForValue("teal-main_ui-modules_ui-root_pca-cluster_columns")
  expect_identical(initial_cluster, FALSE)

  initial_label <- app$waitForValue("teal-main_ui-modules_ui-root_pca-show_matrix")
  expect_identical(initial_matrix, TRUE)

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_cor",
    name = "initial_cor_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_cor",
    name = "initial_cor_table.png"
  )

  # Now update experiment name, assay name, cluster & matrix option on correlation tab.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PC and Sample Correlation",
    "teal-main_ui-modules_ui-root_pca-experiment_name" = "hd2",
    "teal-main_ui-modules_ui-root_pca-assay_name" = "voom",
    "teal-main_ui-modules_ui-root_pca-cluster_columns" = TRUE,
    "teal-main_ui-modules_ui-root_pca-show_matrix" = FALSE
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_cor",
    name = "update1_cor_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_cor",
    name = "update1_cor_table.png"
  )

  # Now go back to pca tab and update experiment, assay name, variance % option,
  # label option and matrix option.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PCA",
    "teal-main_ui-modules_ui-root_pca-experiment_name" = "hd2",
    "teal-main_ui-modules_ui-root_pca-assay_name" = "rpkm",
    "teal-main_ui-modules_ui-root_pca-x_var" = "3",
    "teal-main_ui-modules_ui-root_pca-y_var" = "4",
    "teal-main_ui-modules_ui-root_pca-var_pct" = FALSE,
    "teal-main_ui-modules_ui-root_pca-label" = FALSE,
    "teal-main_ui-modules_ui-root_pca-show_matrix" = FALSE
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_pca",
    name = "update2_pca_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_pca",
    name = "update2_pca_table.png"
  )

  # Update experiment / assay (ensure xvar and yvar revert back to PC1 and PC2, assay to counts)
  # and add color_var for pca.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PCA",
    "teal-main_ui-modules_ui-root_pca-experiment_name" = "hd1",
    "teal-main_ui-modules_ui-root_pca-color_var" = "AGE18"
  )

  new_varpct <- app$waitForValue("teal-main_ui-modules_ui-root_pca-assay_name")
  expect_identical(new_varpct, "counts")

  new_xvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-x_var")
  expect_identical(new_xvar, "1")

  new_yvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-y_var")
  expect_identical(new_yvar, "2")

  new_varpct <- app$waitForValue("teal-main_ui-modules_ui-root_pca-var_pct")
  expect_identical(new_varpct, FALSE)

  new_label <- app$waitForValue("teal-main_ui-modules_ui-root_pca-label")
  expect_identical(new_label, FALSE)

  new_matrix <- app$waitForValue("teal-main_ui-modules_ui-root_pca-show_matrix")
  expect_identical(new_matrix, FALSE)

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_pca",
    name = "update3_pca_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_pca",
    name = "update3_pca_table.png"
  )

  # Update xvar yvar in pca to be the same for a validate msg.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-x_var" = "2",
    "teal-main_ui-modules_ui-root_pca-y_var" = "2"
  )

  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_pca-plot_pca", "message")
  expect_identical(plot_message, "please select two different principal components")

  # Update the inputs to PCA tab, hd1, counts, PC3, PC4, and add filters.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PCA",
    "teal-main_ui-modules_ui-root_pca-experiment_name" = "hd1",
    "teal-main_ui-modules_ui-root_pca-assay_name" = "counts",
    "teal-main_ui-modules_ui-root_pca-x_var" = "3",
    "teal-main_ui-modules_ui-root_pca-y_var" = "4",
    "teal-main_ui-modules_ui-root_pca-var_pct" = TRUE,
    "teal-main_ui-modules_ui-root_pca-label" = TRUE,
    "teal-main_ui-modules_ui-root_pca-show_matrix" = TRUE,
    "teal-main_ui-filter_panel-add_MAE_filter-subjects-var_to_add" = "sex"
  )
  app$setInputs(
    "teal-main_ui-filter_panel-add_MAE_filter-subjects-73212989ea55791d16811d753ac43de0-content-selection" = "M"
  )

  # Ensure xvar and yvar get resetted to pc1 and pc2.
  new_xvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-x_var")
  expect_identical(new_xvar, "1")

  new_yvar <- app$waitForValue("teal-main_ui-modules_ui-root_pca-y_var")
  expect_identical(new_yvar, "2")

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_pca",
    name = "update5_pca_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_pca",
    name = "update5_pca_table.png"
  )

  # Update to cor tab.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PC and Sample Correlation"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-plot_cor",
    name = "update5_cor_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = "teal-main_ui-modules_ui-root_pca-table_cor",
    name = "update5_cor_table.png"
  )

  # Update filter to F to get a validate msg.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PCA",
    "teal-main_ui-filter_panel-add_MAE_filter-subjects-73212989ea55791d16811d753ac43de0-content-selection" = "F"
  )

  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_pca-plot_pca", "message")
  expect_identical(plot_message, "Sample size is too small. PCA needs more than 2 samples.")

  # Update to cor tab.
  app$setInputs(
    "teal-main_ui-modules_ui-root_pca-tab_selected" = "PC and Sample Correlation"
  )

  plot_message <- app$waitForOutputElement("teal-main_ui-modules_ui-root_pca-plot_cor", "message")
  expect_identical(plot_message, "Sample size is too small. PCA needs more than 2 samples.")
})
