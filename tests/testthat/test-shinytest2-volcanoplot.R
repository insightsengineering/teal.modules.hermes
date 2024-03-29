# ui_g_volcanoplot ----

test_that("e2e: tm_g_volcanoplot creates expected HTML", {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_volcanoplot(
        label = "volcanoplot",
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

# tm_g_volcanoplot ----

# nolint start

test_that("volcanoplot module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_volcanoplot(
        label = "volcanoplot",
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

  res <- app$get_active_module_input("compare_group-sample_var")
  expect_null(res)

  # check initial message
  res <- app$get_active_module_output("test")
  expect_identical(res$message, "Please select a group variable")

  # Select an initial group variable.
  app$set_module_input("compare_group-sample_var", "AGE18")
  app$wait_for_idle(timeout = 30000)

  app$expect_screenshot(selector = app$active_module_element("test"))

  # Now change the log2_fc_thresh and check that the plot is updated accordingly.
  app$set_module_input("log2_fc_thresh", 8)
  app$wait_for_idle(timeout = 30000)

  app$expect_screenshot(selector = app$active_module_element("test"))
  app$stop()
})

# nolint end
