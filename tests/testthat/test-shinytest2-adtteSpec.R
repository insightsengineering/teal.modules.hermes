# nolint start

test_that("adtteSpecServer module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  ui <- function(id) {
    ns <- NS(id)

    teal.widgets::standard_layout(
      encoding = tags$div(
        uiOutput(ns("experiment_ui")),
        assaySpecInput(ns("assay")),
        geneSpecInput(ns("genes"), funs = list(Mean = colMeans)),
        adtteSpecInput(ns("adtte"))
      ),
      output = verbatimTextOutput(ns("summary"))
    )
  }

  server <- function(id,
                     data,
                     filter_panel_api) {
    moduleServer(id, function(input, output, session) {
      output$experiment_ui <- renderUI({
        experimentSpecInput(session$ns("experiment"), data = data, mae_name = "MAE")
      })
      experiment <- experimentSpecServer(
        "experiment",
        data = data,
        filter_panel_api,
        mae_name = "MAE"
      )
      assay <- assaySpecServer(
        "assay",
        assays = experiment$assays
      )
      genes <- geneSpecServer(
        "genes",
        funs = list(Mean = colMeans),
        gene_choices = experiment$genes
      )
      adtte <- adtteSpecServer(
        "adtte",
        data = data,
        adtte_name = "ADTTE",
        mae_name = "MAE",
        adtte_vars = list(
          aval = "AVAL",
          avalu = "AVALU",
          is_event = "is_event",
          paramcd = "PARAMCD",
          usubjid = "USUBJID"
        ),
        experiment_data = experiment$data,
        experiment_name = experiment$name,
        assay = assay,
        genes = genes,
        probs = reactive({
          0.5
        }) # nolint
      )
      output$summary <- renderPrint({
        binned_adtte_subset <- adtte$binned_adtte_subset()
        summary(binned_adtte_subset)
      })
    })
  }

  adtte <- teal.modules.hermes::rADTTE %>%
    dplyr::mutate(is_event = (.data$CNSR == 0))

  data <- teal_data(
    ADTTE = adtte,
    MAE = hermes::multi_assay_experiment,
    code =
      "adtte <- teal.modules.hermes::rADTTE %>%
        dplyr::mutate(is_event = (.data$CNSR == 0))"
  )

  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "adtteSpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)
  app$expect_no_shiny_error()

  # check initialization
  res <- app$get_values()
  expect_equal(res$input[[ns("experiment-name")]], "hd1")
  expect_equal(res$output[[ns("summary")]]$message, "please select at least one gene")

  # check correct message
  app$set_module_input("genes-genes", "GeneID:28")
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")

  app$set_module_input("adtte-paramcd", "CRSD")
  res <- app$get_active_module_output("summary")
  expect_match(res, "CRSD")

  app$set_module_input("adtte-paramcd", "PFS")
  res <- app$get_active_module_output("summary")
  expect_match(res, "PFS")

  res <- app$get_active_module_input("adtte-paramcd")

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  app$set_module_input("add-ADTTE-filter-var_to_add", "PARAMCD")
  app$set_module_input("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection", "OS")

  app$wait_for_idle()
  # We expect to get a validation message (also a notification box but we cannot test that)
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")
  res <- app$get_active_module_input("adtte-paramcd")
  expect_equal(res, "")

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$set_module_input("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection", c("PFS", "OS"))
  app$wait_for_idle()
  res <- app$get_active_module_output("summary")
  expect_equal(res$message, "please select an endpoint")

  app$stop()
})

# nolint end
