library(shinytest2)

test_that("forest_tte module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "forest_tte",
    name = "forest_tte module works as expected in the test app"
  )
  ns <- module_ns_shiny2(app)
  ns2 <- NS("teal-main_ui-filter_panel")

  app$wait_for_idle()

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
  app$wait_for_idle()

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "cpm")

  # Choose a gene signature.
  app$set_inputs(!!ns("genes-genes") := c("GeneID:101927746", "GeneID:1820"))
  app$wait_for_idle()

  res <- app$get_value(output = ns("plot-plot_main"))
  expect_identical(res$message, "please select an endpoint")

  # Choose an endpoint.
  app$set_inputs(!!ns("adtte-paramcd") := "PFS")

  app$wait_for_idle()
  app$expect_values()
})
