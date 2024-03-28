# assaySpecInput ----

test_that("assaySpecInput creates expected HTML", {
  expect_silent(result <- assaySpecInput(
    "my_assay",
    label_assays = "select assay"
  ))

  expect_class(result, "shiny.tag.list")
  expect_length(result, 2)

  # First element is a div tag
  expect_tag(result[[1]])

  # Second element is the contents of a single js file
  expect_length(result[[2]], 1)
  expect_tag(result[[2]][[1]])
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



  # Validation message because no assays eligible in first experiment.
  app$wait_for_idle(timeout = 20000)
  res <- app$get_active_module_output("result")
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Select the second experiment and see that we can select the right assays.
  app$set_module_input("experiment-name", "hd2")
  app$wait_for_idle()

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "rpkm")

  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"rpkm\"")

  app$set_module_input("assay-name", "voom")
  app$wait_for_idle()
  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"voom\"")

  # Check that cpm should not be available.
  app$set_module_input("assay-name", "cpm")
  app$wait_for_idle()
  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"\"")

  app$stop()
})

# nolint end
