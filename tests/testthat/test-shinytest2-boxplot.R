# ui_g_boxplot ----

test_that("e2e: tm_g_boxplot initializes without errors and snapshot test", {

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_boxplot(
        label = "boxplot",
        mae_name = "MAE",
        .test = .test
      )
    )
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()

  app$expect_screenshot(name = "app")
  app$stop()
})

# tm_g_boxplot ----

# nolint start

test_that("boxplot module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_boxplot(
        label = "boxplot",
        mae_name = "MAE",
        .test = .test
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("strat-sample_var")
  expect_null(res)

  res <- app$get_active_module_input("genes-genes")
  expect_null(res)

  # check initial message
  res <- app$get_active_module_output("table")
  expect_equal(res$message, "please select at least one gene")

  # Do a couple of updates to obtain a plot.
  app$set_module_input("jitter", TRUE)
  app$set_module_input("violin", TRUE)
  app$set_module_input("genes-genes", "GeneID:5205")
  app$set_module_input("strat-sample_var", "COUNTRY")
  app$set_module_input("color-sample_var", "AGE18")


  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("table"))


  app$stop()
})

# nolint end
