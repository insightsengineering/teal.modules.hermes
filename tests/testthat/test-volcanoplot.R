# ui_g_volcanoplot ----

test_that("ui_g_volcanoplot creates expected HTML", {
  mae_name <- "MyMAE"
  data <- list(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- ui_g_volcanoplot(
    id = "testid",
    mae_name = mae_name,
    pre_output = NULL,
    post_output = NULL
  ))
  expect_class(result, "bslib_page")
})

# tm_g_volcanoplot ----

# nolint start

test_that("volcanoplot module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("volcanoplot"),
    name = "volcanoplot",
    variant = platform_variant(),
    load_timeout = 300000,
    seed = default_app_seed
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # check initialization
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("compare_group-sample_var"))
  expect_null(res)

  # check initial message
  res <- app$get_value(output = ns("test"))
  expect_identical(res$message, "Please select a group variable")

  # Select an initial group variable.
  app$set_inputs(!!ns("compare_group-sample_var") := "AGE18")
  app$wait_for_idle(timeout = 30000)

  res <- app$get_value(output = ns("test"))
  expect_snapshot(cat(res))

  # Now change the log2_fc_thresh and check that the plot is updated accordingly.
  app$set_inputs(!!ns("log2_fc_thresh") := 8)
  app$wait_for_idle(timeout = 30000)

  res <- app$get_value(output = ns("test"))
  expect_snapshot(cat(res))
  app$stop()
})

# nolint end
