# ui_g_pca ----

test_that("e2e: tm_g_pca initializes without errors and snapshot test", {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_pca(
        label = "pca",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000
  )
})

# pca Server ----

# nolint start

test_that("pca module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_pca(
        label = "pca",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)

  # Check initial state of encodings.
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("tab_selected")
  expect_identical(res, "PCA")

  res <- app$get_active_module_input("x_var")
  expect_identical(res, "1")

  res <- app$get_active_module_input("y_var")
  expect_identical(res, "2")

  res <- app$get_active_module_input("var_pct")
  expect_true(res)

  res <- app$get_active_module_input("label")
  expect_true(res)

  res <- app$get_active_module_input("show_matrix")
  expect_true(res)

  app$expect_screenshot(selector = app$active_module_element("test_pca"))


  # Add a gene filter and deselect everything and check that it does not crash.
  app$set_module_input("add-MAE-hd1-row_to_add", "symbol")
  app$wait_for_idle()
  app$set_module_input("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection_open", TRUE, allow_no_input_binding_ = TRUE)
  app$set_module_input("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection", character())
  app$set_module_input("active-MAE-hd1-MAE_symbol_hd1_subset-inputs-selection_open", FALSE, allow_no_input_binding_ = TRUE)

  app$wait_for_idle()
  res <- app$get_active_module_output("test_pca")
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # Remove filters
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-hd1-MAE_symbol_hd1_subset-remove"))

  # Update the tab selection.
  app$set_module_input("tab_selected", "PC and Sample Correlation")
  app$wait_for_idle()

  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("cluster_columns")
  expect_false(res)

  res <- app$get_active_module_input("show_matrix")
  expect_true(res)

  app$expect_screenshot(selector = app$active_module_element("test_cor"))


  # Now update experiment name, assay name, cluster & matrix option on correlation tab.
  app$set_module_input("experiment-name", "hd2")
  app$set_module_input("assay-name", "voom")
  app$set_module_input("cluster_columns", TRUE)
  app$set_module_input("show_matrix", FALSE)

  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("test_cor"))


  # Now go back to pca tab and update experiment, assay name, variance % option,
  # label option and matrix option.
  app$set_module_input("tab_selected", "PCA")
  app$set_module_input("assay-name", "rpkm")
  app$set_module_input("x_var", "3")
  app$set_module_input("y_var", "4")
  app$set_module_input("var_pct", FALSE)
  app$set_module_input("label", FALSE)

  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("test_pca"))


  # Update experiment / assay (ensure xvar and yvar revert back to PC1 and PC2, assay to counts)
  # and add color for pca.
  app$set_module_input("experiment-name", "hd1")
  app$wait_for_idle()
  app$set_module_input("color-sample_var", "AGE18")
  app$wait_for_idle()

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("x_var")
  expect_identical(res, "1")

  res <- app$get_active_module_input("y_var")
  expect_identical(res, "2")

  res <- app$get_active_module_input("var_pct")
  expect_false(res)

  res <- app$get_active_module_input("label")
  expect_false(res)

  res <- app$get_active_module_input("show_matrix")
  expect_false(res)

  # Update xvar yvar in pca to be the same for a validate msg.
  app$set_module_input("x_var", "2")
  app$set_module_input("y_var", "2")


  res <- app$wait_for_active_module_value(output = "plot_pca-plot_main")
  expect_identical(res$message, "please select two different principal components")

  # Update the inputs to PCA tab, hd1, counts, PC3, PC4, and add filters.
  app$set_module_input("x_var", "3")
  app$set_module_input("y_var", "4")
  app$set_module_input("var_pct", TRUE)
  app$set_module_input("label", TRUE)
  app$set_module_input("show_matrix", TRUE)

  app$set_module_input("add-MAE-subjects-var_to_add", "SEX")
  app$wait_for_idle(timeout = 40000)
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection", "M")

  # Ensure xvar and yvar get resetted to pc1 and pc2.
  app$wait_for_idle(timeout = 20000)
  res <- app$get_active_module_input("x_var")
  expect_identical(res, "1")
  res <- app$get_active_module_input("y_var")
  expect_identical(res, "2")


  app$expect_screenshot(selector = app$active_module_element("test_pca"))

  # Update to cor tab.
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection_open", TRUE, allow_no_input_binding_ = TRUE)
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection", "F")
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection_open", FALSE, allow_no_input_binding_ = TRUE)

  app$wait_for_idle()
  res <- app$get_active_module_output("test_pca")
  expect_identical(res$message, "Sample size is too small. PCA needs more than 2 samples.")

  # Remove filter.
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-subjects-MAE_SEX-remove"))

  # Initiate the use of Top Variance Genes filtering functionality.
  app$set_module_input("filter_top", TRUE)
  res <- app$wait_for_active_module_value(input = "n_top")
  expect_identical(res, 500L)

  app$expect_screenshot(selector = app$active_module_element("test_pca"))

  # Change the number of top genes.
  app$set_module_input("n_top", 777L)

  # Change to another experiment and check that it did not change.
  app$set_module_input("experiment-name", "hd2")
  app$wait_for_idle()

  res <- app$get_active_module_input("n_top")
  expect_identical(res, 777L)

  # Increase number of top genes to maximum.
  app$set_module_input("n_top", 2500L)
  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("test_pca"))

  # Switch off gene filtering and check that table is still the same.
  app$set_module_input("filter_top", FALSE)
  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("test_pca"))

  # Go back to first experiment and check that n_top stayed the same.
  app$set_module_input("experiment-name", "hd1")
  res <- app$wait_for_active_module_value(input = "n_top")
  expect_identical(res, 2500L)
  app$stop()
})

# nolint end
