# ui_g_quality ----

test_that("ui_g_quality creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- ui_g_quality(
    id = "testid",
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))

  expect_tag(result)
})

# tm_g_quality ----

# nolint start

test_that("quality module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("quality"),
    name = "quality",
    variant = platform_variant(),
    load_timeout = 300000
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
  res <- app$wait_for_value(output = ns("table"))
  expect_identical(res$message, "Please change gene filters to ensure that there are at least 2 genes")

  # Initial plot.
  res <- app$get_active_module_output("table")
  expect_snapshot(
    res
  )

  # Choose another experiment.
  app$set_module_input("experiment-name", "hd3")
  app$set_module_input("min_depth", "Specify")

  # Check state of encodings again.
  res <- app$wait_for_value(input = ns("min_cpm"))
  expect_identical(res, 26L)

  res <- app$wait_for_value(input = ns("min_depth_continuous"))
  expect_identical(res, 1777260L)

  # Final histogram plot.
  res <- app$get_active_module_output("table")
  expect_snapshot(
    cat(res)
  )

  # Change to another plot type so that we can choose another assay.
  app$set_module_input("plot_type", "Top Genes Plot")
  app$set_module_input("assay-name", "cpm")
  app$wait_for_idle(timeout = 30000)
  res <- app$get_active_module_output("table")
  expect_snapshot(
    cat(res)
  )
  app$stop()
})

# nolint end
