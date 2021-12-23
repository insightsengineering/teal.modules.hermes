# geneSpecInput ----

test_that("geneSpecInput creates expected HTML", {
  expect_snapshot(geneSpecInput(
    "my_genes",
    funs = list(mean = colMeans),
    label_funs = "Please select function"
  ))
})

# h_parse_genes ----

test_that("h_parse_genes works as expected", {
  choices <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("a", "b", "c", "d", "e")
  )
  result <- h_parse_genes(c("2535", "a", "bla"), choices)
  expected <- choices[c(1, 3), ]
  expect_identical(result, expected)
})

test_that("h_parse_genes correctly returns empty data frame when no genes match", {
  choices <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("a", "b", "c", "d", "e")
  )
  result <- h_parse_genes(c("2539", "x", "bla"), choices)
  expected <- choices[NULL, ]
  expect_identical(result, expected)
})

# geneSpecServer ----

test_that("geneSpec module works as expected in the test app", {
  skip_if_covr()
  utils.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("geneSpec"),
    loadTimeout = 1e5,
    debug = "all", phantomTimeout = 1e5, seed = 123
  )
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  ns <- module_ns(app)

  # Initially no genes are selected.
  initial_genes <- app$waitForValue(ns("my_genes"), ignore = "")
  expect_identical(initial_genes, NULL)

  # Validation message points towards gene selection.
  output_message <- app$waitForOutputElement(ns("result"), "message")
  expect_identical(output_message, "please select at least one gene")

  # Set genes manually.
  selected_genes <- c(
    "GeneID:101927746", "GeneID:1820", "GeneID:101929818", "GeneID:94115", "GeneID:25959",
    "GeneID:140459", "GeneID:6618", "GeneID:51264", "GeneID:127254", "GeneID:139604",
    "GeneID:102724552", "GeneID:356", "GeneID:101928458", "GeneID:2576", "GeneID:84651",
    "GeneID:3423", "GeneID:5810", "GeneID:9302", "GeneID:285847", "GeneID:9166", "GeneID:101927628",
    "GeneID:1499", "GeneID:6098", "GeneID:83858", "GeneID:100303749", "GeneID:4608"
  )
  app$setValue(ns("my_genes-genes"), selected_genes)

  # We see that now the first function is selected.
  fun_name <- app$waitForValue(ns("my_genes-fun_name"))
  expect_identical(fun_name, "mean")

  # Then we get the expected result.
  output_text <- app$waitForValue(ns("result"), iotype = "output")
  expect_identical(output_text, "mean(ARID3A, ASB6, ..., SPINK7)")

  # Now we add chromosome filters for the first experiment.
  ns2 <- NS("teal-main_ui-filter_panel-add_MAE_filter-hd1")
  app$setValue(
    ns2("row_to_add"),
    "chromosome"
  )

  # Now we lock the gene selection.
  app$setValue(ns("my_genes-lock_button"), TRUE)

  # Now we just select a small subset of chromosomes.
  app$waitForValue(ns2("rowData_var_chromosome-content-selection"))
  app$setValue(
    ns2("rowData_var_chromosome-content-selection"),
    c("X", "Y")
  )

  # Confirm that gene selection was not changed.
  genes_while_locked <- app$waitForValue(ns("my_genes-genes"))
  expect_set_equal(genes_while_locked, selected_genes)
  # Note: Due to sorting by gene name the order is not the same.

  # Check validation message.
  output_message <- app$waitForOutputElement(ns("result"), "message")
  expect_identical(
    output_message,
    "23 genes (GeneID:1820, GeneID:140459, ..., GeneID:84651) not included, please unlock or change filters"
  )

  # Now we unlock.
  app$setValue(ns("my_genes-lock_button"), FALSE)

  # Check that gene selection was reduced accordingly.
  genes_after_unlock <- app$waitForValue(ns("my_genes-genes"))
  expect_set_equal(genes_after_unlock, c("GeneID:139604", "GeneID:2576", "GeneID:3423"))
  expect_true(all(genes_after_unlock %in% selected_genes))
  expect_length(setdiff(selected_genes, genes_after_unlock), 23L)

  # Then we get the expected result.
  output_text <- app$waitForValue(ns("result"), iotype = "output")
  expect_identical(output_text, "mean(GAGE4, IDS, MAGEB16)")

  # Now we remove the filter.
  app$click(ns2("rowData_var_chromosome-remove"))

  # We select a gene via text input.
  app$click(ns("my_genes-text_button"))
  app$waitForValue(ns("my_genes-ok_button"))
  app$setValue(ns("my_genes-gene_text"), "GeneID:100303749; GeneID:4608")
  app$click(ns("my_genes-ok_button"))

  # Confirm that they are now selected.
  Sys.sleep(1) # Need this here otherwise we don't get latest update below.
  genes_after_text <- app$waitForValue(ns("my_genes-genes"))
  expect_set_equal(genes_after_text, c("GeneID:100303749", "GeneID:4608"))
})

# validate_gene_spec ----

test_that("validate_gene_spec works as expected", {
  expect_error(
    validate_gene_spec(hermes::gene_spec(), letters),
    "please select at least one gene"
  )
  gene_spec <- hermes::gene_spec(c("a", "b", "z"))
  expect_silent(validate_gene_spec(gene_spec, letters))
  expect_error(
    validate_gene_spec(gene_spec, letters[1:3]),
    "1 gene (z) not included",
    fixed = TRUE
  )
})
