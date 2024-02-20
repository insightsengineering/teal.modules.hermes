# ui_g_scatterplot ----

test_that("ui_g_scatterplot creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- teal.data::teal_data(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- ui_g_scatterplot(
    id = "testid",
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  ))

  expect_tag(result)
})

# tm_g_scatterplot ----

# nolint start

test_that("scatterplot module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("scatterplot"),
    name = "scatterplot",
    variant = platform_variant(),
    load_timeout = 300000,
    seed = default_app_seed
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # check initialization
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(output = ns("plot-plot_out_main"))
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Choose another experiment.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  res <- app$wait_for_value(input = ns("assay-name"))
  expect_identical(res, "cpm")

  app$wait_for_idle()
  res <- app$get_value(input = ns("x_spec-genes"))
  expect_null(res)

  res <- app$get_value(input = ns("y_spec-genes"))
  expect_null(res)

  res <- app$get_value(output = ns("plot-plot_out_main"))
  expect_identical(res$message, "please select at least one gene")

  # Set one gene each.
  app$set_inputs(!!ns("x_spec-genes") := "GeneID:503538")
  app$set_inputs(!!ns("y_spec-genes") := "GeneID:8086")

  app$wait_for_idle()

  # Change the sample filter and confirm that genes are not updated.
  app$set_inputs(!!ns2("add-MAE-subjects-var_to_add") := "SEX")
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection") := "F")

  res <- app$get_value(input = ns("x_spec-genes"))
  expect_identical(res, "GeneID:503538")
  res <- app$wait_for_value(input = ns("y_spec-genes"))
  expect_identical(res, "GeneID:8086")

  # Remove sample filter
  app$click(ns2("active-MAE-subjects-MAE_SEX-remove"))
  app$wait_for_idle()

  res <- app$get_value(input = ns("x_spec-genes"))
  expect_identical(res, "GeneID:503538")
  res <- app$wait_for_value(input = ns("y_spec-genes"))
  expect_identical(res, "GeneID:8086")

  # Now change the experiment_name, genes, method.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$set_inputs(!!ns("x_spec-genes") := "GeneID:441376")
  app$set_inputs(!!ns("y_spec-genes") := "GeneID:79963")
  app$set_inputs(!!ns("smooth_method") := "loess")
  app$set_inputs(!!ns("facet_var-sample_var") := "AGE18")

  app$wait_for_idle()
  app$expect_select_screenshot(ns("plot-plot_out_main"))
})

# nolint end
