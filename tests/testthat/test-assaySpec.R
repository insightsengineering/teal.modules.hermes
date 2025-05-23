# assaySpecInput ----

test_that("assaySpecInput creates expected HTML", {
  expect_silent(result <- assaySpecInput(
    "my_assay",
    label_assays = "select assay"
  ))

  expect_class(result, "shiny.tag")
})

# nolint start

# assaySpecServer ----
test_that("assaySpecServer module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("assaySpec"),
    name = "assaySpec",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)

  ns <- module_ns_shiny2(app)

  # Validation message because no assays eligible in first experiment.
  res <- app$get_value(output = ns("result"))
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Select the second experiment and see that we can select the right assays.
  app$set_inputs(!!ns("experiment-name") := "hd2")
  app$wait_for_idle()

  res <- app$get_value(input = ns("assay-name"))
  expect_identical(res, "rpkm")

  res <- app$get_value(output = ns("result"))
  expect_identical(res, "[1] \"rpkm\"")

  app$set_inputs(!!ns("assay-name") := "voom")
  app$wait_for_idle()
  res <- app$get_value(output = ns("result"))
  expect_identical(res, "[1] \"voom\"")

  # Check that cpm should not be available.
  app$set_inputs(!!ns("assay-name") := "cpm")
  app$wait_for_idle()
  res <- app$get_value(output = ns("result"))
  expect_identical(res, "[1] \"\"")

  app$stop()
})

# nolint end
