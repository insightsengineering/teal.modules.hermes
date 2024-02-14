# ui_g_km ----

test_that("ui_g_km creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- ui_g_km(
    id = "testid",
    adtte_name = "ADTTE",
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  ))

  expect_tag(result)
})

# tm_g_km ----

# nolint start

test_that("km module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("km"),
    name = "km",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # Check initial state of encodings.
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(output = ns("plot-plot_out_main"))
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Choose another experiment.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$wait_for_idle()

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "cpm")

  # Choose a gene signature.
  app$set_inputs(!!ns("genes-genes") := c("GeneID:10061", "GeneID:28"))
  app$wait_for_idle()

  # Choose an endpoint.
  res <- app$get_value(output = ns("plot-plot_out_main"))
  expect_identical(res$message, "please select an endpoint")
  app$set_inputs(!!ns("adtte-paramcd") := "PFS")
  app$wait_for_idle()

  app$expect_select_screenshot(ns("plot-plot_out_main"))
})

# nolint end
