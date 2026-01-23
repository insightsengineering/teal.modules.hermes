# geneSpecInput ----

test_that("geneSpecInput creates expected HTML", {
  expect_silent(result <- geneSpecInput(
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
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("geneSpec"),
    name = "geneSpec",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)

  ns <- module_ns_shiny2(app)

  res <- app$get_value(input = ns("my_genes-genes"))
  expect_null(res)

  res <- app$get_value(output = ns("result"))
  expect_identical(res$message, "please select at least one gene")

  # Set genes manually.
  selected_genes <- c(
    "GeneID:10061", "GeneID:28", "GeneID:47", "GeneID:8310",
    "GeneID:52", "GeneID:88", "GeneID:11096"
  )

  app$set_inputs(!!ns("my_genes-genes") := selected_genes)
  app$wait_for_idle()

  # See that now the first function is selected.
  res <- app$get_value(input = ns("my_genes-fun_name"))
  expect_identical(res, "mean")

  # Get the expected result.
  res <- app$get_value(output = ns("result"))
  expect_identical(res, "mean(ABCF2, ABO, ..., ADAMTS5)")


  # lock the gene selection
  app$set_inputs(!!ns("my_genes-lock_button") := TRUE)

  # Filter data using the right side panel
  ns_fp <- active_module_filter_panel_ns(app)
  app$set_inputs(!!ns_fp("MAE-MAE-hd1-row_to_add") := "chromosome", allow_no_input_binding_ = TRUE)
  app$set_inputs(!!ns_fp("MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection") := c("1", "2"))
  app$set_inputs(
    !!ns_fp("MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection_open") := FALSE,
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()

  # Confirm that gene selection was not changed.
  # Note: Due to sorting by gene name the order might not be the same.
  res <- app$get_value(input = ns("my_genes-genes"))
  expect_set_equal(res, selected_genes)

  res <- app$get_value(output = ns("result"))
  expect_identical(
    res$message,
    "5 genes (GeneID:10061, GeneID:28, ..., GeneID:11096) not included, please unlock or change filters"
  )

  # Unlock the gene selection.
  app$set_inputs(!!ns("my_genes-lock_button") := FALSE)
  app$wait_for_idle()

  # Check that gene selection was reduced accordingly.
  res <- app$get_value(input = ns("my_genes-genes"))
  expect_set_equal(res, c("GeneID:52", "GeneID:88"))
  expect_subset(res, selected_genes)
  expect_length(setdiff(selected_genes, res), 5)

  # Get the expected result.
  res <- app$get_value(output = ns("result"))
  expect_identical(res, "mean(ACP1, ACTN2)")

  # Remove the filter.
  app$click(ns_fp("MAE-hd1-MAE_chromosome_hd1_subset-remove"))

  # Select a gene via text input.
  app$click(ns("my_genes-text_button"))
  app$wait_for_idle()
  app$set_inputs(!!ns("my_genes-gene_text") := "GeneID:10061; GeneID:28")
  app$click(ns("my_genes-ok_button"))
  app$wait_for_idle()
  res <- app$get_value(input = ns("my_genes-genes"))
  expect_set_equal(res, c("GeneID:10061", "GeneID:28"))

  app$stop()
})
