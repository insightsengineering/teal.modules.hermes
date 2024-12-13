library(dplyr)
library(teal.modules.hermes)

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

my_app <- function() {
  adtte <- teal.data::rADTTE %>%
    dplyr::mutate(is_event = (.data$CNSR == 0))

  data <- teal_data(
    ADTTE = adtte,
    MAE = hermes::multi_assay_experiment,
    code =
      "adtte <- teal.data::rADTTE %>%
        dplyr::mutate(is_event = (.data$CNSR == 0))"
  )

  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "adtteSpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
