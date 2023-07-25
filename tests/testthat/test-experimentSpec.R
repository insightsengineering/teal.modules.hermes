# experimentSpecInput ----
test_that("experimentSpecInput creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  data <- list(MyMAE = function() hermes::multi_assay_experiment)
  expect_silent(result <- experimentSpecInput(
    inputId = "my_experiment",
    data = data,
    mae_name = mae_name,
    label_experiments = "Please select the best experiment"
  ))

  expect_tag(result)
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

# nolint start

test_that("experimentSpec module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("experimentSpec"),
    name = "experimentSpec",
    variant = platform_variant()
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

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
  app$set_inputs(!!ns2("add-MAE-subjects-var_to_add") := "SEX")
  app$wait_for_idle()
  app$set_inputs(!!ns2("active-MAE-subjects-MAE_SEX-inputs-selection") := character())
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")
  app$click(ns2("active-MAE-remove_filters"))

  # Same for filtering out all genes.
  app$set_inputs(!!ns2("add-MAE-hd2-row_to_add") := "chromosome")
  app$wait_for_idle()
  app$set_inputs(
    !!ns2("active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection_open") := TRUE,
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(!!ns2("active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection") := character(0))
  app$set_inputs(
    !!ns2("active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection_open") := FALSE,
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # return to initial situation
  app$click(ns2("active-MAE-remove_filters"))
  app$wait_for_idle()

  res <- app$get_value(output = ns("summary"))
  expect_match(res, "HermesData object with 9 samples of 2500 genes")
})

# nolint end
