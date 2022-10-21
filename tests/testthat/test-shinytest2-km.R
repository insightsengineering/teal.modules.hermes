# ui_g_km ----

test_that("ui_g_km creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_km(
    id = "testid",
    datasets = datasets,
    adtte_name = "ADTTE",
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_km ----

test_that("km module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = "km",
    name = "km module works as expected in the test app",
    variant = platform_variant()
  )
  ns <- module_ns_shiny2(app)

  app$wait_for_idle()

  # Check initial state of encodings.
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(output = ns("km_plot"))
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
  res <- app$get_value(output = ns("km_plot"))
  expect_identical(res$message, "please select an endpoint")
  app$set_inputs(!!ns("adtte-paramcd") := "PFS")
  app$wait_for_idle()

  app$expect_screenshot()
})
