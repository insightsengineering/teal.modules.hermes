library(shinytest2)

test_that("barplot module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "barplot",
    name = "barplot module works as expected in the test app"
  )
  ns <- module_ns_shiny2(app)
  ns2 <- NS("teal-main_ui-filter_panel")

  app$wait_for_idle()

  # check initialization
  res <- app$get_value(input = ns("experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "counts")

  res <- app$get_value(input = ns("genes"))
  expect_null(res)

  # check initial message
  res <- app$get_value(output = ns("plot"))
  expect_equal(res$message, "please select at least one gene")

  # Set values
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$set_inputs(!!ns("assay-name") := "tmp")
  app$set_inputs(!!ns("x-genes") := "GeneID:8086")

  app$wait_for_idle()
  res <- app$get_value(input = ns("x-genes"))
  expect_identical(res, "GeneID:8086")

  app$set_inputs(!!ns("experiment-name") := "hd1")

  # Check that gene list is updated
  app$wait_for_idle()
  res <- app$get_value(input = ns("x-genes"))
  expect_null(res)

  # Check that assay list is updated
  res <- app$get_value(input = ns("assay"))
  expect_null(res)

  # Check error message in case of identical percentile boundaries
  app$set_inputs(!!ns("percentiles") := c(0.1, 0.1))
  app$wait_for_idle()

  res <- app$get_value(output = ns("plot"))
  expect_equal(
    res$message,
    "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
  )

  # Set Experiment, assay, gene, percentile and facet
  app$set_inputs(
    !!ns("experiment-name") := "hd1",
    !!ns("assay-name") := "counts",
    !!ns("x-genes") := "GeneID:47",
    !!ns("percentiles") := c(0.2, 0.8),
    !!ns("facet-sample_var") := "AGE18"
  )

  app$expect_values()
})
