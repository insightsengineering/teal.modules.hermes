# assaySpecInput ----

test_that("assaySpecInput creates expected HTML", {
  expect_snapshot(assaySpecInput(
    "my_assay",
    label_assays = "Please select the best assay"
  ))
})

# assaySpecServer ----

test_that("assaySpec module works as expected in the test app", {
  skip_if_covr()
  utils.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("assaySpec"), loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(app$getSource()), css = ".teal_module"),
    "id"
  )
  ns <- NS(module_id)

  # Validation message because no assays eligible in first experiment.
  output_message <- app$waitForOutputElement(ns("result"), "message")
  expect_identical(
    output_message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Select the second experiment and see that we can select the right assays.
  app$setValue(ns("experiment-name"), "hd2")

  assay2 <- app$waitForValue(ns("assay-name"))
  expect_identical(assay2, "rpkm")
  result2 <- app$waitForValue(ns("result"), iotype = "output")
  expect_match(result2, "rpkm")

  app$setValue(ns("assay-name"), "voom")
  assay3 <- app$waitForValue(ns("assay-name"))
  expect_identical(assay3, "voom")

  app$setValue(ns("assay-name"), "cpm")
  assay4 <- app$waitForValue(ns("assay-name"), ignore = NULL)
  expect_identical(assay4, "")  # Because cpm should not be available.
})
