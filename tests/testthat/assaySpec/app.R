library(teal.modules.hermes)

ui <- function(id,
               datasets) {
  ns <- NS(id)

  teal.devel::standard_layout(
    encoding = div(
      experimentSpecInput(
        ns("experiment"),
        datasets = datasets,
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
                   datasets) {
  moduleServer(id, function(input, output, session) {
    experiment <- experimentSpecServer(
      "experiment",
      datasets = datasets,
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
  mae_data <- dataset(mae_name, mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "assaySpec example",
        server = server,
        ui = ui,
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
