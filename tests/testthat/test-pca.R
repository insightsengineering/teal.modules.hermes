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
    load_timeout = 300000,
    seed = default_app_seed
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

  res <- app$get_value(output = ns("test_pca"))
  expect_snapshot(
    cat(res)
  )

  # Add a gene filter and deselect everything and check that it does not crash.
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-MAE-hd1-row_to_add` = "chromosome", allow_no_input_binding_ = TRUE)
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection` = character())
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection_open` = FALSE, allow_no_input_binding_ = TRUE)
  app$wait_for_idle()

  res <- app$get_value(output = ns("test_pca"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # Remove filters
  app$click("teal-teal_modules-pca-filter_panel-filters-MAE-hd1-MAE_chromosome_hd1_subset-remove")
  app$wait_for_idle()

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

  res <- app$get_value(output = ns("test_cor"))
  expect_snapshot(
    cat(res)
  )

  # Now update experiment name, assay name, cluster & matrix option on correlation tab.
  app$set_inputs(!!ns("experiment-name") := "hd2", timeout_ = 30000)
  app$set_inputs(!!ns("assay-name") := "voom")
  app$set_inputs(!!ns("cluster_columns") := TRUE)
  app$set_inputs(!!ns("show_matrix") := FALSE)

  app$wait_for_idle()
  res <- app$get_value(output = ns("test_cor"))
  expect_snapshot(
    cat(res)
  )

  # Now go back to pca tab and update experiment, assay name, variance % option,
  # label option and matrix option.
  app$set_inputs(!!ns("tab_selected") := "PCA")
  app$set_inputs(!!ns("assay-name") := "rpkm")
  app$wait_for_idle(timeout = 30000) # Important to ensure update of x_var.
  app$set_inputs(!!ns("x_var") := "3")
  app$set_inputs(!!ns("y_var") := "4")
  app$set_inputs(!!ns("var_pct") := FALSE)
  app$set_inputs(!!ns("label") := FALSE)

  app$wait_for_idle(timeout = 30000)
  res <- app$get_value(input = ns("x_var"))
  expect_identical(res, "3")
  res <- app$get_value(input = ns("y_var"))
  expect_identical(res, "4")

  res <- app$get_value(output = ns("test_pca"))
  expect_snapshot(
    cat(res)
  )

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
  app$set_inputs(!!ns("x_var") := "3")
  app$set_inputs(!!ns("y_var") := "4")
  app$set_inputs(!!ns("var_pct") := TRUE)
  app$set_inputs(!!ns("label") := TRUE)
  app$set_inputs(!!ns("show_matrix") := TRUE)

  app$wait_for_idle(timeout = 40000)
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-MAE-subjects-var_to_add` = "SEX", allow_no_input_binding_ = TRUE)
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-subjects-MAE_SEX-inputs-selection` = "F")
  app$set_inputs(`teal-teal_modules-pca-filter_panel-filters-MAE-subjects-MAE_SEX-inputs-selection_open` = FALSE, allow_no_input_binding_ = TRUE)

  # Ensure xvar and yvar DONT get reset.
  app$wait_for_idle(timeout = 20000)
  res <- app$get_value(input = ns("x_var"))
  expect_identical(res, "3")
  res <- app$get_value(input = ns("y_var"))
  expect_identical(res, "4")

  app$wait_for_idle()
  res <- app$get_value(output = ns("test_pca"))
  expect_identical(res$message, "Sample size is too small. PCA needs more than 2 samples.")

  # Remove filter.
  app$click("teal-teal_modules-pca-filter_panel-filters-MAE-subjects-MAE_SEX-remove")

  # Initiate the use of Top Variance Genes filtering functionality.
  app$set_inputs(!!ns("filter_top") := TRUE)
  res <- app$wait_for_value(input = ns("n_top"))
  expect_identical(res, 500L)

  res <- app$get_value(output = ns("test_pca"))
  expect_snapshot(
    cat(res)
  )

  # Change the number of top genes.
  app$set_inputs(!!ns("n_top") := 777L)

  # Change to another experiment and check that it did not change.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$wait_for_idle(timeout = 20000)

  res <- app$get_value(input = ns("n_top"))
  expect_identical(res, 777L)

  # Increase number of top genes to maximum.
  app$set_inputs(!!ns("n_top") := 2500L)
  app$wait_for_idle()
  res <- app$get_value(output = ns("test_pca"))
  expect_snapshot(
    cat(res)
  )

  # Switch off gene filtering and check that table is still the same.
  app$set_inputs(!!ns("filter_top") := FALSE)
  app$wait_for_idle()
  res <- app$get_value(output = ns("test_pca"))
  expect_snapshot(
    cat(res)
  )

  # Go back to first experiment and check that n_top stayed the same.
  app$set_inputs(!!ns("experiment-name") := "hd1")
  res <- app$wait_for_value(input = ns("n_top"))
  expect_identical(res, 2500L)
  app$stop()
})

# nolint end
