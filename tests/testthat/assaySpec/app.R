library(teal.modules.hermes)

ui <- function(id,
               datasets) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
      experimentSpecInput(
        ns("experiment"),
        datasets,
        "MAE"
      ),
      assaySpecInput(
        ns("assay"),
        label_assays = "Please choose assay"
      )
    ),
    output = textOutput(ns("result"))
  )
}

server <- function(input,
                   output,
                   session,
                   datasets) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets,
    "MAE"
  )
  assay <- assaySpecServer(
    "assay",
    experiment$assays,
    exclude_assays = c("counts", "cpm", "tpm", "bla")
  )
  output$result <- renderPrint({
    assay()
  })
}

my_app <- function() {
  mae <- hermes::multi_assay_experiment
  mae_name <- "MAE"
  mae_data <- dataset(mae_name, mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
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
