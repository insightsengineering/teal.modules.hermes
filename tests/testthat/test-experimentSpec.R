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

# h_order_genes ----

test_that("h_order_genes works as expected to sort only by name", {
  genes <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("e", "d", "c", "b", "a")
  )
  result <- h_order_genes(genes)
  expected <- genes[5:1, ]
  expect_identical(result, expected)
})

test_that("h_order_genes works as expected to sort only by id", {
  genes <- data.frame(
    id = c("7", "1", "2", "345346", "0"),
    name = rep("", 5)
  )
  result <- h_order_genes(genes)
  expected <- genes[c(5, 2, 3, 4, 1), ] # Note: alphabetical sorting.
  expect_identical(result, expected)
})

test_that("h_order_genes works as expected with mixed id and name sorting", {
  genes <- data.frame(
    id = c("7", "1", "2", "345346", "0"),
    name = c("e", "", "c", "", "a")
  )
  result <- h_order_genes(genes)
  expected <- genes[c(5, 3, 1, 2, 4), ]
  expect_identical(result, expected)
})

test_that("h_order_genes does not fail when given empty data frame", {
  genes <- data.frame(
    id = character(),
    name = character()
  )
  result <- h_order_genes(genes)
  expect_identical(result, genes)
})

# h_gene_data ----

test_that("h_gene_data works as expected", {
  object <- hermes::hermes_data[1:10, ]
  result <- h_gene_data(object, "symbol")
  expected <- data.frame(
    id = c(
      "GeneID:11185", "GeneID:10677", "GeneID:101928428",
      "GeneID:100422835", "GeneID:102466731", "GeneID:64881", "GeneID:286205",
      "GeneID:8365", "GeneID:6804", "GeneID:100423018"
    ),
    name = c(
      "INMT", "AVIL", "LOC101928428", "MIR3183", "MIR6769A", "PCDH20",
      "SCAI", "HIST1H4H", "STX1A", "MIR3156-3"
    )
  )
  expect_identical(result, expected)
})

test_that("h_gene_data does not fail when object does not contain any genes", {
  object <- hermes::hermes_data[NULL, ]
  result <- expect_silent(h_gene_data(object, "symbol"))
  expected <- data.frame(
    id = character(),
    name = character()
  )
  expect_identical(result, expected)
})

# experimentSpecServer ----

test_that("experimentSpec module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("experimentSpec"),
    loadTimeout = 1e5,
    debug = "all", phantomTimeout = 1e5, seed = 123
  )
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  ns <- module_ns(app)

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
    "              id  name\n249 GeneID:10061 ABCF2\n966    GeneID:28   ABO\n532"
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

  # Filtering out all samples does give a validation message, so we are safe
  # downstream.
  ns2 <- NS("teal-main_ui-filter_panel")
  app$setValue(ns2("add_MAE_filter-subjects-var_to_add"), "SEX")
  app$waitForValue(ns2("MAE_filter-subjects-_var_SEX-remove"))
  app$setValue(ns2("MAE_filter-subjects-_var_SEX-content-selection"), character())
  expect_match(
    app$waitForOutputElement(ns("summary"), "message"),
    "No genes or samples included in this experiment, please adjust filters"
  )
  app$click(ns2("MAE_filter-remove_filters"))

  # Same for filtering out all genes.
  app$setValue(ns2("add_MAE_filter-hd2-row_to_add"), "chromosome")
  app$waitForValue(ns2("MAE_filter-hd2-rowData_var_chromosome-content-selection"))
  app$setValue(ns2("MAE_filter-hd2-rowData_var_chromosome-content-selection"), character())
  expect_match(
    app$waitForOutputElement(ns("summary"), "message"),
    "No genes or samples included in this experiment, please adjust filters"
  )
  app$click(ns2("MAE_filter-remove_filters"))
})
