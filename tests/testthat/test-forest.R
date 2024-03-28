# ui_g_forest_tte ----

test_that("ui_g_forest_tte creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- ui_g_forest_tte(
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

# tm_g_forest_tte ----

# nolint start

test_that("forest_tte module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("forest_tte"),
    name = "forest_tte",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_output("table")
  expect_identical(
    res$message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$set_module_input("experiment-name", "hd2")

  res <- app$wait_for_value(input = ns("assay-name"))
  expect_identical(res, "cpm")

  # Choose a gene signature.
  app$set_module_input("genes-genes", c("GeneID:101927746", "GeneID:1820"))

  res <- app$wait_for_value(output = ns("plot-plot_out_main"))
  expect_identical(res$message, "please select an endpoint")

  # Choose an endpoint.
  app$set_module_input("adtte-paramcd", "PFS")

  app$wait_for_idle()
  res <- app$get_active_module_output("table")
  expect_snapshot(
    cat(res)
  )

  app$stop()
})

# nolint end
