library(shinytest2)

test_that("experimentSpec module works as expected in the test app", {
  app <- AppDriver$new(
    app_dir = "experimentSpec",
    name = "experimentSpec module works as expected in the test app"
  )
  ns <- module_ns_shiny2(app)
  ns2 <- NS("teal-main_ui-filter_panel")

  app$wait_for_idle()

  # check initialization
  res <- app$get_value(input = ns("my_experiment-name"))
  expect_identical(res, "hd1")

  res <- app$get_value(input = ns("property"))
  expect_identical(res, "data")

  # The data is correctly processed so we can see the print result.
  app$wait_for_idle()
  res <- app$get_value(output = ns("head"))
  expect_match(res, "class: HermesData\nassays(1): counts", fixed = TRUE)

  # Look at the other properties and confirm that they are ok.
  app$set_inputs(!!ns("property") := "name")
  app$wait_for_idle()

  res <- app$get_value(output = ns("head"))
  expect_match(res, "hd1", fixed = TRUE)

  app$set_inputs(!!ns("property") := "genes")
  app$wait_for_idle()

  res <- app$get_value(output = ns("head"))
  expect_match(res, "              id  name\n249 GeneID:10061 ABCF2\n966    GeneID:28   ABO\n532", fixed = TRUE)

  app$set_inputs(!!ns("property") := "assays")
  app$wait_for_idle()

  res <- app$get_value(output = ns("head"))
  expect_match(res, "counts", fixed = TRUE)

  # Look at the second experiment.
  app$set_inputs(!!ns("my_experiment-name") := "hd2")
  app$wait_for_idle()

  res <- app$get_value(output = ns("head"))
  expect_match(res, '"counts"\\s+"cpm"\\s+"rpkm"\\s+"tpm"\\s+"voom"')

  app$set_inputs(!!ns("property") := "data")
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res, "HermesData object with 9 samples of 2500 genes")

  # Filtering out all samples does give a validation message, so we are safe
  # downstream.
  app$set_inputs(!!ns2("add_MAE_filter-subjects-var_to_add") := "SEX")
  app$wait_for_idle()
  app$set_inputs(!!ns2("MAE_filter-subjects-_var_SEX-content-selection") := character())
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")
  app$click(ns2("MAE_filter-remove_filters"))

  # Same for filtering out all genes.
  app$set_inputs(!!ns2("add_MAE_filter-hd2-row_to_add") := "chromosome")
  app$wait_for_idle()
  app$set_inputs(!!ns2("MAE_filter-hd2-rowData_var_chromosome-content-selection") := character())
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # return to initial situation
  app$click(ns2("MAE_filter-remove_filters"))
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res, "HermesData object with 9 samples of 2500 genes")
})

