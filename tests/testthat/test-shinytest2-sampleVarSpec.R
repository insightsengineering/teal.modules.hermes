library(shinytest2)

test_that("sampleVarSpec module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "sampleVarSpec",
    name = "sampleVarSpec module works as expected in the test app",
    variant = platform_variant()
  )
  ns <- module_ns_shiny2(app)

  app$wait_for_idle()

  # Initially no variable is selected.
  res <- app$get_value(input = ns("facet_var"))
  expect_null(res)

  # Select a variable.
  first_var <- "AGE18"
  app$set_inputs(!!ns("facet_var-sample_var") := first_var)

  # Check the output and which levels are reported there.
  res <- app$wait_for_value(output = ns("summary"))
  expect_match(res, " < 18 >= 18 \n")

  # Now click on the levels button, set combination and click ok.
  app$click(ns("facet_var-levels_button"))
  first_combination <- list("< 18" = "2", ">= 18" = "2")
  # Click on second column in both rows.
  app$wait_for_idle()
  app$set_inputs(!!ns("facet_var-comb_assignment") := first_combination)
  app$wait_for_idle()
  app$click(ns("facet_var-ok"))

  # Check the output and which levels are reported there.
  res <- app$wait_for_value(output = ns("summary"))
  expect_match(res, "< 18/>= 18 \n")
})
