# nolint start

test_that("experimentSpec module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  ui <- function(id) {
    ns <- NS(id)
    teal.widgets::standard_layout(
      encoding = tags$div(
        uiOutput(ns("experiment_ui")),
        selectInput(
          ns("property"),
          "Please choose property",
          c("data", "name", "genes", "assays")
        )
      ),
      output = tags$div(
        verbatimTextOutput(ns("summary")),
        verbatimTextOutput(ns("head"))
      )
    )
  }

  server <- function(id,
                     data,
                     filter_panel_api,
                     mae_name) {
    moduleServer(id, function(input, output, session) {
      output$experiment_ui <- renderUI({
        experimentSpecInput(
          session$ns("my_experiment"),
          data,
          mae_name,
          label_experiments = "Please choose experiment"
        )
      })
      experiment <- experimentSpecServer(
        "my_experiment",
        data,
        filter_panel_api,
        mae_name
      )
      result <- reactive({
        switch(input$property,
               data = experiment$data(),
               name = experiment$name(),
               genes = experiment$genes(),
               assays = experiment$assays()
        )
      })
      output$summary <- renderPrint({
        result <- result()
        hermes::summary(result)
      })
      output$head <- renderPrint({
        result <- result()
        utils::head(result)
      })
    })
  }

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "experimentSpec example",
        server = server,
        server_args = list(mae_name = "MAE"),
        ui = ui,
        datanames = "all"
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_active_module_input("my_experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_input("property")
  expect_identical(res, "data")

  # The data is correctly processed so we can see the print result.
  app$wait_for_idle()
  res <- app$get_active_module_output("head")
  expect_match(res, "class: HermesData\nassays(1): counts", fixed = TRUE)

  # Look at the other properties and confirm that they are ok.
  app$set_module_input("property", "name")
  app$wait_for_idle()

  res <- app$get_active_module_output("head")
  expect_match(res, "hd1", fixed = TRUE)

  app$set_module_input("property", "genes")
  app$wait_for_idle()

  res <- app$get_active_module_output("head")
  expect_match(res, "              id  name\n249 GeneID:10061 ABCF2\n966    GeneID:28   ABO\n532", fixed = TRUE)

  app$set_module_input("property", "assays")
  app$wait_for_idle()

  res <- app$get_active_module_output("head")
  expect_match(res, "counts", fixed = TRUE)

  # Look at the second experiment.
  app$set_module_input("my_experiment-name", "hd2")
  app$wait_for_idle()

  res <- app$get_active_module_output("head")
  expect_match(res, '"counts"\\s+"cpm"\\s+"rpkm"\\s+"tpm"\\s+"voom"')

  app$set_module_input("property", "data")
  app$wait_for_idle()

  res <- app$get_active_module_output("summary")
  expect_match(res, "HermesData object with 9 samples of 2500 genes")

  # Filtering out all samples does give a validation message, so we are safe
  # downstream.
  app$set_module_input("add-MAE-subjects-var_to_add", "SEX")
  app$wait_for_idle()
  app$set_module_input("active-MAE-subjects-MAE_SEX-inputs-selection", character())
  app$wait_for_idle()

  # Experiment selection is not affected by filtering
  res <- app$get_active_module_input("my_experiment-name")
  expect_identical(res, "hd2")

  res <- app$get_active_module_output("summary")
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-remove_filters"))

  # Same for filtering out all genes.
  app$set_module_input("add-MAE-hd2-row_to_add", "chromosome")
  app$wait_for_idle()
  app$set_module_input(
    "active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection_open", TRUE,
    allow_no_input_binding_ = TRUE
  )
  app$set_module_input("active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection", character(0))
  app$set_module_input(
    "active-MAE-hd2-MAE_chromosome_hd2_subset-inputs-selection_open", FALSE,
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()

  # Experiment selection is not affected by filtering
  res <- app$get_active_module_input("my_experiment-name")
  expect_identical(res, "hd2")

  res <- app$get_active_module_output("summary")
  expect_match(res$message, "No genes or samples included in this experiment, please adjust filters")

  # return to initial situation
  app$click(sprintf("%s-%s", app$active_filters_ns(), "active-MAE-remove_filters"))
  app$wait_for_idle()

  # Experiment selection is not affected by removing filters
  res <- app$get_active_module_input("my_experiment-name")
  expect_identical(res, "hd2")

  res <- app$get_active_module_output("summary")
  expect_match(res, "HermesData object with 9 samples of 2500 genes")

  app$stop()
})

# nolint end
