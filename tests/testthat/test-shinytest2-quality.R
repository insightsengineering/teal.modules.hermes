# ui_g_quality ----

test_that("e2e: tm_g_quality initializes without errors and snapshot test", {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_quality(
        label = "quality",
        mae_name = "MAE",
        .test = TRUE
      )
    )
  )

  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()

  app$expect_screenshot(name = "app")
  app$stop()
})

# tm_g_quality ----

# nolint start

test_that("e2e: quality module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_quality(
        label = "quality",
        mae_name = "MAE",
        .test = TRUE
      )
    )
  )

  app$wait_for_idle(timeout = 20000)

  # Check initial state of encodings.
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "counts")

  res <- app$get_active_module_input("plot_type")
  expect_identical(res, "Histogram")

  # Check that warning message for at least 2 genes works as expected.
  app$set_module_input("min_cpm", 54356)
  res <- app$wait_for_active_module_value(output = "table")
  expect_identical(res$message, "Please change gene filters to ensure that there are at least 2 genes")

  # Initial plot.
  app$expect_screenshot(selector = app$active_module_element("table"))

  # Choose another experiment.
  app$set_module_input("experiment-name", "hd3")
  app$set_module_input("min_depth", "Specify")

  # Check state of encodings again.
  res <- app$wait_for_active_module_value(input = "min_cpm")
  expect_identical(res, 26L)

  res <- app$wait_for_active_module_value(input = "min_depth_continuous")
  expect_identical(res, 1777260L)

  # Final histogram plot.
  app$expect_screenshot(selector = app$active_module_element("table"))

  # Change to another plot type so that we can choose another assay.
  app$set_module_input("plot_type", "Top Genes Plot")
  app$set_module_input("assay-name", "cpm")
  app$wait_for_idle(timeout = 30000)
  app$expect_screenshot(selector = app$active_module_element("table"))
  app$stop()
})

# nolint end
