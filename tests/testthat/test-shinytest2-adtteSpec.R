# nolint start

test_that("adtteSpecServer module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("adtteSpec"),
    name = "adtteSpecServe",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_values()
  expect_equal(res$input[[ns("experiment-name")]], "hd1")
  expect_equal(res$output[[ns("summary")]]$message, "please select at least one gene")

  # check correct message
  app$set_module_input("genes-genes", "GeneID:28")
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")

  app$set_module_input("adtte-paramcd", "CRSD")
  res <- app$get_active_module_output("summary")
  expect_match(res, "CRSD")

  app$set_module_input("adtte-paramcd", "PFS")
  res <- app$get_active_module_output("summary")
  expect_match(res, "PFS")

  res <- app$get_active_module_input("adtte-paramcd")

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  app$set_module_input("add-ADTTE-filter-var_to_add", "PARAMCD")
  app$set_module_input("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection", "OS")

  app$wait_for_idle()
  # We expect to get a validation message (also a notification box but we cannot test that)
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")
  res <- app$get_active_module_input("adtte-paramcd")
  expect_equal(res, "")

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$set_module_input("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection", c("PFS", "OS"))
  app$wait_for_idle()
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")

  app$stop()
})

# nolint end
