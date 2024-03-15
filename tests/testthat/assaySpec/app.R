library(teal.modules.hermes)

ui <- function(id) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = tags$div(
      uiOutput(ns("experiment_ui")),
      assaySpecInput(
        ns("assay"),
        label_assays = "Please choose assay"
      )
    ),
    output = verbatimTextOutput(ns("result"))
  )
}

server <- function(id,
                   data,
                   filter_panel_api,
                   mae_name) {
  moduleServer(id, function(input, output, session) {
    output$experiment_ui <- renderUI({
      experimentSpecInput(
        session$ns("experiment"),
        data = data,
        mae_name = "MAE"
      )
    })
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
      mae_name = "MAE"
    )
    assay <- assaySpecServer(
      "assay",
      assays = experiment$assays,
      exclude_assays = c("counts", "cpm", "tpm", "bla")
    )
    output$result <- renderPrint({
      assay <- assay()
      assay
    })
  })
}

my_app <- function() {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      module(
        label = "assaySpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
