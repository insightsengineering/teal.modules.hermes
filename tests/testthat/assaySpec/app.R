library(teal.modules.hermes)

ui <- function(id,
               data) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(
        ns("experiment"),
        data = data,
        mae_name = "MAE"
      ),
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
                   filter_panel_api) {
  moduleServer(id, function(input, output, session) {
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
  mae <- hermes::multi_assay_experiment
  mae_name <- "MAE"
  mae_data <- teal.data::dataset(mae_name, mae)
  data <- teal.data::teal_data(mae_data)
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
