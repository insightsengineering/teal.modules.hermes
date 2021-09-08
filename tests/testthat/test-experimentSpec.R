# experimentSpecInput ----

test_that("experimentSpecInput creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(experimentSpecInput(
    "my_experiment",
    datasets = datasets,
    mae_name = mae_name,
    label_experiments = "Please select the best experiment"
  ))
})

# experimentSpecServer ----

test_that("experimentSpec module works as expected in the test app", {
  skip_if_covr()
  test.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new("experimentSpec/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  # nolint start

  ns <- NS("teal-main_ui-modules_ui-root_experimentSpec_example")

  # Initially the first experiment is selected.
  initial_experiment <- app$waitForValue(ns("my_experiment-name"))
  expect_identical(initial_experiment, "hd1")

  # And we are looking at the data in the output.
  initial_property <- app$waitForValue(ns("property"))
  expect_identical(initial_property, "data")

  # The data is correctly processed so we can see the print result.
  head_result <- app$waitForValue(ns("head"), iotype = "output")
  expect_match(head_result, "class: HermesData\nassays(1): counts", fixed = TRUE)

  # Look at the other properties and confirm that they are ok.
  app$setValue(ns("property"), "name")
  expect_match(
    app$waitForValue(ns("head"), iotype = "output"),
    "hd1"
  )
  app$setValue(ns("property"), "genes")
  expect_match(
    app$waitForValue(ns("head"), iotype = "output"),
    '"GeneID:101927746"\\s+"GeneID:1820"\\s+"GeneID:101929818"\\s+"GeneID:94115"'
  )
  app$setValue(ns("property"), "assays")
  expect_match(
    app$waitForValue(ns("head"), iotype = "output"),
    "counts"
  )

  # Look at the second experiment.
  app$setValue(ns("my_experiment-name"), "hd2")
  expect_match(
    app$waitForValue(ns("head"), iotype = "output"),
    '"counts"\\s+"cpm"\\s+"rpkm"\\s+"tpm"\\s+"voom"'
  )
  app$setValue(ns("property"), "data")
  expect_match(
    app$waitForValue(ns("summary"), iotype = "output"),
    "HermesData object with 9 samples of 2500 genes"
  )

  # Filtering out all samples does not lead to an error but an empty experiment.
  ns2 <- NS("teal-main_ui-filter_panel-add_MAE_filter")
  app$setValue(ns2("subjects-var_to_add"), "sex")
  app$setValue(ns2("subjects-var_sex-content-selection"), character())
  expect_match(
    app$waitForValue(ns("summary"), iotype = "output"),
    "HermesData object with 0 samples of 2500 genes"
  )
  app$click("teal-main_ui-filter_panel-MAE_filters-remove_filters")

  # Same for filtering out all genes.
  app$setValue(ns2("hd2-row_to_add"), "Chromosome")
  app$setValue(ns2("hd2-rowData_var_Chromosome-content-selection"), character())
  expect_match(
    app$waitForValue(ns("summary"), iotype = "output"),
    "HermesData object with 9 samples of 0 genes"
  )
  app$click("teal-main_ui-filter_panel-MAE_filters-remove_filters")

  app$stop()
})
