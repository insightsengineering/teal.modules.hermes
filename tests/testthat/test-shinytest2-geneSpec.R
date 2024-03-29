# nolint start
test_that("geneSpec module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  ui <- function(id, funs) {
    ns <- NS(id)
    teal.widgets::standard_layout(
      encoding = tags$div(
        geneSpecInput(
          ns("my_genes"),
          funs = funs,
          label_funs = "Please select function"
        )
      ),
      output = textOutput(ns("result"))
    )
  }

  server <- function(id,
                     data,
                     funs) {
    moduleServer(id, function(input, output, session) {
      gene_choices <- reactive({
        mae <- data()[["MAE"]]
        object <- mae[[1]]
        gene_ids <- rownames(object)
        gene_names <- SummarizedExperiment::rowData(object)$symbol
        gene_data <- data.frame(
          id = gene_ids,
          name = gene_names
        )
        gene_data[order(gene_data$name), ]
      })
      gene_spec <- geneSpecServer(
        "my_genes",
        funs = funs,
        gene_choices = gene_choices
      )
      output$result <- renderText({
        validate_gene_spec(
          gene_spec(),
          gene_choices()$id
        )
        gene_spec <- gene_spec()
        gene_spec$get_label()
      })
    })
  }

  funs <- list(mean = colMeans)

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "GeneSpec example",
        server = server,
        server_args = list(funs = funs),
        ui = ui,
        ui_args = list(funs = funs),
        datanames = "all"
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)

  res <- app$get_active_module_input("my_genes-genes")
  expect_null(res)

  res <- app$get_active_module_output("result")
  expect_identical(res$message, "please select at least one gene")

  # Set genes manually.
  selected_genes <- c(
    "GeneID:10061", "GeneID:28", "GeneID:47", "GeneID:8310",
    "GeneID:52", "GeneID:88", "GeneID:11096"
  )

  app$set_module_input("my_genes-genes", selected_genes)
  app$wait_for_idle()

  # See that now the first function is selected.
  res <- app$get_active_module_input("my_genes-fun_name")
  expect_identical(res, "mean")

  # Get the expected result.
  res <- app$get_active_module_output("result")
  expect_identical(res, "mean(ABCF2, ABO, ..., ADAMTS5)")

  # Add chromosome filters for the first experiment.
  app$set_module_input("add-MAE-hd1-row_to_add", "chromosome")

  # Lock the gene selection.
  app$set_module_input("my_genes-lock_button", TRUE)
  app$wait_for_idle()
  app$set_module_input(
    "active-MAE-hd2-MAE_chromosome_hd1_subset-inputs-selection_open", TRUE,
    allow_no_input_binding_ = TRUE
  )
  app$set_module_input("active-MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection", c("1", "2"))
  app$set_module_input(
    "active-MAE-hd1-MAE_chromosome_hd1_subset-inputs-selection_open", FALSE,
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()

  # Confirm that gene selection was not changed.
  # Note: Due to sorting by gene name the order might not be the same.
  res <- app$get_active_module_input("my_genes-genes")
  expect_set_equal(res, selected_genes)

  res <- app$get_active_module_output("result")
  expect_identical(
    res$message,
    "5 genes (GeneID:10061, GeneID:28, ..., GeneID:11096) not included, please unlock or change filters"
  )

  # Unlock the gene selection.
  app$set_module_input("my_genes-lock_button", FALSE)
  app$wait_for_idle()

  # Check that gene selection was reduced accordingly.
  res <- app$get_active_module_input("my_genes-genes")
  expect_set_equal(res, c("GeneID:52", "GeneID:88"))
  expect_subset(res, selected_genes)
  expect_length(setdiff(selected_genes, res), 5)

  # Get the expected result.
  res <- app$get_active_module_output("result")
  expect_identical(res, "mean(ACP1, ACTN2)")

  # Remove the filter.
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-hd1-MAE_chromosome_hd1_subset-remove"))

  # Select a gene via text input.
  app$click(ns("my_genes-text_button"))
  app$wait_for_idle()
  app$set_module_input("my_genes-gene_text", "GeneID:10061; GeneID:28")
  app$click(ns("my_genes-ok_button"))
  app$wait_for_idle()
  res <- app$get_active_module_input("my_genes-genes")
  expect_set_equal(res, c("GeneID:10061", "GeneID:28"))

  app$stop()
})

# nolint end
