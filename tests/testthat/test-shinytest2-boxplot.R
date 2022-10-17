library(shinytest2)

test_that("boxplot module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "boxplot",
    name = "boxplot module works as expected in the test app"
  )
  ns <- module_ns_shiny2(app)
  ns2 <- NS("teal-main_ui-filter_panel")

  app$wait_for_idle()

  # check initialization
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "counts")

  res <- app$get_value(input = ns("strat-sample_var"))
  expect_null(res)

  res <- app$get_value(input = ns("genes-genes"))
  expect_null(res)

  # check initial message
  res <- app$get_value(output = ns("plot"))
  expect_equal(res$message, "please select at least one gene")

  # Do a couple of updates to obtain a plot.
  app$set_inputs(
    !!ns("jitter") := TRUE,
    !!ns("violin") := TRUE,
    !!ns("genes-genes") := "GeneID:5205",
    !!ns("strat-sample_var") := "COUNTRY",
    !!ns("color-sample_var") := "AGE18"
  )

  app$expect_values()
})
