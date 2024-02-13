# ui_g_pca ----

test_that("ui_g_pca creates HTML", {
  mae_name <- "MyMAE"
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  result <- ui_g_pca(
    id = "testid",
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  )
  testthat::expect_s3_class(result, "shiny.tag.list")
})

# pca Server ----

# nolint start

test_that("pca module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("pca"),
    name = "pca",
    variant = platform_variant(),
    load_timeout = 30000
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # Check initial state of encodings.
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "counts")

  res <- app$get_value(input = ns("tab_selected"))
  expect_identical(res, "PCA")

  res <- app$get_value(input = ns("x_var"))
  expect_identical(res, "1")

  res <- app$get_value(input = ns("y_var"))
  expect_identical(res, "2")

  res <- app$get_value(input = ns("var_pct"))
  expect_true(res)

  res <- app$get_value(input = ns("label"))
  expect_true(res)

  res <- app$get_value(input = ns("show_matrix"))
  expect_true(res)

  app$expect_select_screenshot(ns("plot_pca-plot_main"))

  # Add a gene filter and deselect everything and check that it does not crash.
  app$set_inputs(!!ns2("add-MAE-hd1-row_to_add") := "symbol")
  app$wait_for_idle()
  app$set_inputs(!!ns2("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection_open") := TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(!!ns2("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection") := character())
  app$set_inputs(!!ns2("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection_open") := FALSE, allow_no_input_binding_ = TRUE)

  app$wait_for_idle()
  res <- app$get_value(output = ns("plot_pca-plot_main"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # Remove filters
  app$click(ns2("active-MAE-hd1-MAE_symbol_hd1_subset-remove"))

  # Update the tab selection.
  app$set_inputs(!!ns("tab_selected") := "PC and Sample Correlation")
  app$wait_for_idle()

  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "counts")

  res <- app$get_value(input = ns("cluster_columns"))
  expect_false(res)

  res <- app$get_value(input = ns("show_matrix"))
  expect_true(res)

  app$expect_select_screenshot(ns("plot_cor-plot_main"))
  app$expect_select_screenshot(ns("table_cor"))

  # Now update experiment name, assay name, cluster & matrix option on correlation tab.
  app$set_inputs(
    !!ns("experiment-name") := "hd2",
    !!ns("assay-name") := "voom",
    !!ns("cluster_columns") := TRUE,
    !!ns("show_matrix") := FALSE
  )

  # app$wait_for_idle()
  app$expect_select_screenshot(ns("plot_cor-plot_main"))

  # Now go back to pca tab and update experiment, assay name, variance % option,
  # label option and matrix option.
  app$set_inputs(
    !!ns("tab_selected") := "PCA",
    !!ns("assay-name") := "rpkm",
    !!ns("x_var") := "3",
    !!ns("y_var") := "4",
    !!ns("var_pct") := FALSE,
    !!ns("label") := FALSE,
    !!ns("show_matrix") := FALSE
  )

  app$wait_for_idle()
  app$expect_select_screenshot(ns("plot_pca-plot_main"))
  app$expect_select_screenshot(ns("table_pca"))

  # Update experiment / assay (ensure xvar and yvar revert back to PC1 and PC2, assay to counts)
  # and add color for pca.
  app$set_inputs(!!ns("experiment-name") := "hd1")
  app$wait_for_idle()
  app$set_inputs(!!ns("color-sample_var") := "AGE18")
  app$wait_for_idle()

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "counts")

  res <- app$get_value(input = ns("x_var"))
  expect_identical(res, "1")

  res <- app$get_value(input = ns("y_var"))
  expect_identical(res, "2")

  res <- app$get_value(input = ns("var_pct"))
  expect_false(res)

  res <- app$get_value(input = ns("label"))
  expect_false(res)

  res <- app$get_value(input = ns("show_matrix"))
  expect_false(res)

  # Update xvar yvar in pca to be the same for a validate msg.
  app$set_inputs(
    !!ns("x_var") := "2",
    !!ns("y_var") := "2"
  )

  res <- app$wait_for_value(output = ns("plot_pca-plot_main"))
  expect_identical(res$message, "please select two different principal components")

  # Update the inputs to PCA tab, hd1, counts, PC3, PC4, and add filters.
  app$set_inputs(
    !!ns("tab_selected") := "PCA",
    !!ns("experiment-name") := "hd1",
    !!ns("assay-name") := "counts",
    !!ns("x_var") := "3",
    !!ns("y_var") := "4",
    !!ns("var_pct") := TRUE,
    !!ns("label") := TRUE,
    !!ns("show_matrix") := TRUE
  )

  app$set_inputs(!!ns2("add-MAE-subjects-var_to_add") := "SEX")
  app$wait_for_idle()
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection") := "M")

  # Ensure xvar and yvar get resetted to pc1 and pc2.
  app$wait_for_idle(timeout = 20000)
  res <- app$get_value(input = ns("x_var"))
  expect_identical(res, "1")
  res <- app$get_value(input = ns("y_var"))
  expect_identical(res, "2")

  app$expect_select_screenshot(ns("plot_pca-plot_main"))

  # Update to cor tab.
  app$set_inputs(!!ns("tab_selected") := "PCA")
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection_open") := TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection") := "F")
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection_open") := FALSE, allow_no_input_binding_ = TRUE)

  app$wait_for_idle()
  res <- app$get_value(output = ns("plot_pca-plot_main"))
  expect_identical(res$message, "Sample size is too small. PCA needs more than 2 samples.")

  # Remove filter.
  app$click(ns2("active-MAE-subjects-MAE_SEX-remove"))

  # Initiate the use of Top Variance Genes filtering functionality.
  app$set_inputs(!!ns("filter_top") := TRUE)
  res <- app$wait_for_value(input = ns("n_top"))
  expect_identical(res, 500L)

  app$expect_select_screenshot(ns("plot_pca-plot_main"))

  # Change the number of top genes.
  app$set_inputs(!!ns("n_top") := 777L)

  # Change to another experiment and check that it did not change.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$wait_for_idle()

  res <- app$get_value(input = ns("n_top"))
  expect_identical(res, 777L)

  # Increase number of top genes to maximum.
  app$set_inputs(!!ns("n_top") := 2500L)
  app$wait_for_idle()
  app$expect_select_screenshot(ns("plot_pca-plot_main"))

  # Switch off gene filtering and check that table is still the same.
  app$set_inputs(!!ns("filter_top") := FALSE)
  app$wait_for_idle()
  app$expect_select_screenshot(ns("plot_pca-plot_main"))

  # Go back to first experiment and check that n_top stayed the same.
  app$set_inputs(!!ns("experiment-name") := "hd1")
  app$set_inputs(!!ns("filter_top") := "TRUE")
  res <- app$wait_for_value(input = ns("n_top"))
  expect_identical(res, 2500L)
})

# nolint end
