# ui_g_forest_tte ----

test_that("ui_g_forest_tte creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- list(MyMAE = function() hermes::multi_assay_experiment)
  result <- ui_g_forest_tte(
    id = "testid",
    data = data,
    adtte_name = "ADTTE",
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  )
  expect_tag(result)
})

# tm_g_forest_tte ----

# nolint start

test_that("forest_tte module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = "forest_tte",
    name = "forest_tte module works as expected in the test app",
    variant = platform_variant()
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # check initialization
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(output = ns("plot-plot_main"))
  expect_identical(
    res$message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$set_inputs(!!ns("experiment-name") := "hd2")

  res <- app$wait_for_value(input = ns("assay-name"))
  expect_identical(res, "cpm")

  # Choose a gene signature.
  app$set_inputs(!!ns("genes-genes") := c("GeneID:101927746", "GeneID:1820"))

  res <- app$wait_for_value(output = ns("plot-plot_main"))
  expect_identical(res$message, "please select an endpoint")

  # Choose an endpoint.
  app$set_inputs(!!ns("adtte-paramcd") := "PFS")

  app$wait_for_idle()
  app$expect_screenshot()
})

# nolint end
