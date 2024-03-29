# ui_g_scatterplot ----

test_that("e2e: tm_g_scatterplot creates expected HTML", {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = teal_data(MAE = helios::large_helios_data),
    modules = teal::modules(
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000,
    seed = default_app_seed
  )

  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()

  app$expect_screenshot(name = "app")
  app$stop()

})

# tm_g_scatterplot ----

# nolint start

test_that("e2e: scatterplot module works as expected", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = teal_data(MAE = helios::large_helios_data),
    modules = teal::modules(
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000,
    seed = default_app_seed
  )

  app$wait_for_idle(timeout = 20000)

  # check initialization
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_output("table")
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Choose another experiment.
  app$set_module_input("experiment-name", "hd2")
  res <- app$wait_for_active_module_value(input = "assay-name")
  expect_identical(res, "cpm")

  app$wait_for_idle()
  res <- app$get_active_module_input("x_spec-genes")
  expect_null(res)

  res <- app$get_active_module_input("y_spec-genes")
  expect_null(res)

  res <- app$get_active_module_output("table")
  expect_identical(res$message, "please select at least one gene")

  # Set one gene each.
  app$set_module_input("x_spec-genes", "GeneID:503538")
  app$set_module_input("y_spec-genes", "GeneID:8086")

  app$wait_for_idle()

  # Change the sample filter and confirm that genes are not updated.
  app$set_module_input("add-MAE-subjects-var_to_add", "SEX")
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection", "F")

  res <- app$get_active_module_input("x_spec-genes")
  expect_identical(res, "GeneID:503538")
  res <- app$wait_for_active_module_value(input = "y_spec-genes")
  expect_identical(res, "GeneID:8086")

  # Remove sample filter
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-subjects-MAE_SEX-remove"))
  app$wait_for_idle()

  res <- app$get_active_module_input("x_spec-genes")
  expect_identical(res, "GeneID:503538")
  res <- app$wait_for_active_module_value(input = "y_spec-genes")
  expect_identical(res, "GeneID:8086")

  # Now change the experiment_name, genes, method.
  app$set_module_input("experiment-name", "hd2")
  app$set_module_input("x_spec-genes", "GeneID:441376")
  app$set_module_input("y_spec-genes", "GeneID:79963")
  app$set_module_input("smooth_method", "loess")
  app$set_module_input("facet_var-sample_var", "AGE18")

  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("table"))

  app$stop()
})

# nolint end
