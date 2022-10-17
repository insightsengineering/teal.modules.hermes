library(shinytest2)

test_that("{shinytest2} recording: adtteSpecServer module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "adtteSpec",
    name = "adtteSpecServer module works as expected in the test app"
  )
  ns <- module_ns_shiny2(app)
  ns2 <- NS("teal-main_ui-filter_panel")

  # check initialization
  res <- app$get_values()
  expect_equal(res$input[[ns("experiment-name")]], "hd1")
  expect_equal(res$output[[ns("summary")]]$message, "please select at least one gene")

  # check correct message
  app$set_inputs(!!ns("genes-genes") := "GeneID:28")
  res <- app$get_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")

  app$set_inputs(!!ns("adtte-paramcd") := "CRSD")
  res <- app$get_value(output = ns("summary"))
  expect_match(res, "CRSD")

  app$set_inputs(!!ns("adtte-paramcd") := "PFS")
  res <- app$get_value(output = ns("summary"))
  expect_match(res, "PFS")

  res <- app$get_value(input = ns("adtte-paramcd"))

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  app$set_inputs(!!ns2("add_ADTTE_filter-filter-var_to_add") := "PARAMCD")
  app$set_inputs(!!ns2("ADTTE_filter-filter-_var_PARAMCD-content-selection") := "OS")

  app$wait_for_idle()
  # We expect to get a validation message (also a notification box but we cannot test that)
  res <- app$wait_for_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")
  res <- app$get_value(input = ns("adtte-paramcd"))
  expect_equal(res, "")

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$set_inputs(!!ns2("ADTTE_filter-filter-_var_PARAMCD-content-selection") := c("PFS", "OS"))
  res <- app$wait_for_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")
})
