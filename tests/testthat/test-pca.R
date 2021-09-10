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

  ns <- NS("teal-main_ui-modules_ui-root_pca")

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue(ns("experiment_name"))
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "counts")

  initial_tab <- app$waitForValue(ns("tab_selected"))
  expect_identical(initial_tab, "PCA")

  initial_xvar <- app$waitForValue(ns("x_var"))
  expect_identical(initial_xvar, "1")

  initial_yvar <- app$waitForValue(ns("y_var"))
  expect_identical(initial_yvar, "2")

  initial_varpct <- app$waitForValue(ns("var_pct"))
  expect_identical(initial_varpct, TRUE)

  initial_label <- app$waitForValue(ns("label"))
  expect_identical(initial_label, TRUE)

  initial_matrix <- app$waitForValue(ns("show_matrix"))
  expect_identical(initial_matrix, TRUE)

  expect_snapshot_screenshot(
    app,
    id = ns("plot_pca"),
    name = "initial_pca_plot.png"
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_pca"),
    name = "initial_pca_table.png"
  )

  # Now update the tab selection.
  app$setValue(ns("tab_selected"), "PC and Sample Correlation")

  initial_experiment_name <- app$waitForValue(ns("experiment_name"))
  expect_identical(initial_experiment_name, "hd1")

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "counts")

  initial_cluster <- app$waitForValue(ns("cluster_columns"))
  expect_identical(initial_cluster, FALSE)

  initial_label <- app$waitForValue(ns("show_matrix"))
  expect_identical(initial_matrix, TRUE)

  expect_snapshot_screenshot(
    app,
    id = ns("plot_cor"),
    name = "initial_cor_plot.png",
    wait_for_plot = TRUE
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_cor"),
    name = "initial_cor_table.png"
  )

  # Now update experiment name, assay name, cluster & matrix option on correlation tab.
  app$setValue(ns("experiment_name"), "hd2")
  app$setValue(ns("assay-name"), "voom")
  app$setValue(ns("cluster_columns"), TRUE)
  app$setValue(ns("show_matrix"), FALSE)

  expect_snapshot_screenshot(
    app,
    id = ns("plot_cor"),
    name = "update1_cor_plot.png",
    wait_for_plot = TRUE
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_cor"),
    name = "update1_cor_table.png"
  )

  # Now go back to pca tab and update experiment, assay name, variance % option,
  # label option and matrix option.
  app$setValue(ns("tab_selected"), "PCA")
  app$setValue(ns("assay-name"), "rpkm")
  app$setValue(ns("x_var"), "3")
  app$setValue(ns("y_var"), "4")
  app$setValue(ns("var_pct"), FALSE)
  app$setValue(ns("label"), FALSE)
  app$setValue(ns("show_matrix"), FALSE)

  expect_snapshot_screenshot(
    app,
    id = ns("plot_pca"),
    name = "update2_pca_plot.png",
    wait_for_plot = TRUE
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_pca"),
    name = "update2_pca_table.png"
  )

  # Update experiment / assay (ensure xvar and yvar revert back to PC1 and PC2, assay to counts)
  # and add color_var for pca.
  app$setValue(ns("experiment_name"), "hd1")
  app$setValue(ns("color_var"), "AGE18")

  new_varpct <- app$waitForValue(ns("assay-name"))
  expect_identical(new_varpct, "counts")

  new_xvar <- app$waitForValue(ns("x_var"))
  expect_identical(new_xvar, "1")

  new_yvar <- app$waitForValue(ns("y_var"))
  expect_identical(new_yvar, "2")

  new_varpct <- app$waitForValue(ns("var_pct"))
  expect_identical(new_varpct, FALSE)

  new_label <- app$waitForValue(ns("label"))
  expect_identical(new_label, FALSE)

  new_matrix <- app$waitForValue(ns("show_matrix"))
  expect_identical(new_matrix, FALSE)

  expect_snapshot_screenshot(
    app,
    id = ns("plot_pca"),
    name = "update3_pca_plot.png",
    wait_for_plot = TRUE
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_pca"),
    name = "update3_pca_table.png"
  )

  # Update xvar yvar in pca to be the same for a validate msg.
  app$setValue(ns("x_var"), "2")
  app$setValue(ns("y_var"), "2")

  plot_message <- app$waitForOutputElement(ns("plot_pca"), "message")
  expect_identical(plot_message, "please select two different principal components")

  # Update the inputs to PCA tab, hd1, counts, PC3, PC4, and add filters.
  app$setValues(
    ns = ns,
    "tab_selected" = "PCA",
    "experiment_name" = "hd1",
    "assay-name" = "counts",
    "x_var" = "3",
    "y_var" = "4",
    "var_pct" = TRUE,
    "label" = TRUE,
    "show_matrix" = TRUE
  )

  ns2 <- NS("teal-main_ui-filter_panel-add_MAE_filter")
  app$setValue(ns2("subjects-var_to_add"), "sex")
  # Before selecting, it seems we need to wait a bit for the initial state.
  app$waitForValue(ns2("subjects-var_sex-content-selection"))
  app$setValue(ns2("subjects-var_sex-content-selection"), "M")

  # Ensure xvar and yvar get resetted to pc1 and pc2.
  new_xvar <- app$waitForValue(ns("x_var"))
  expect_identical(new_xvar, "1")

  new_yvar <- app$waitForValue(ns("y_var"))
  expect_identical(new_yvar, "2")

  expect_snapshot_screenshot(
    app,
    id = ns("plot_pca"),
    name = "update5_pca_plot.png",
    wait_for_plot = TRUE
  )

  expect_snapshot_screenshot(
    app,
    id = ns("table_pca"),
    name = "update5_pca_table.png"
  )

  # Update to cor tab.
  app$setValue(ns("tab_selected"), "PC and Sample Correlation")

  # Check that correct validation message is displayed.
  plot_message <- app$waitForOutputElement(ns("plot_cor"), "message")
  expect_identical(plot_message, "Obtained NA results in the correlation matrix, therefore no plot can be produced")

  expect_snapshot_screenshot(
    app,
    id = ns("table_cor"),
    name = "update5_cor_table.png"
  )

  # Update filter to F, look at PCA plot, to get another validate msg.
  app$setValue(ns("tab_selected"), "PCA")
  app$setValue(ns2("subjects-var_sex-content-selection"), "F")

  plot_message <- app$waitForOutputElement(ns("plot_pca"), "message")
  expect_identical(plot_message, "Sample size is too small. PCA needs more than 2 samples.")

  # Initiate the use of Top Variance Genes function
  # app$setValue(ns("filter_top"), TRUE)
  app$setInputs(`teal-main_ui-modules_ui-root_pca-n_top` = 500)

  expect_snapshot_screenshot(
    app,
    id = ns("table_cor"),
    name = "update5_cor_table.png"
  )

})
