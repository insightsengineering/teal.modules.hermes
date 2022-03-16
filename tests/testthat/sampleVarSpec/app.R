library(teal.modules.hermes)

ui <- function(id,
               datasets) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), datasets, "MAE"),
      sampleVarSpecInput(ns("facet_var"), "Select variable")
    ),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(id,
                   datasets) {
  moduleServer(id, function(input, output, session) {
    experiment <- experimentSpecServer(
      "experiment",
      datasets,
      "MAE"
    )
    facet_var_spec <- sampleVarSpecServer(
      "facet_var",
      experiment_name = experiment$name,
      original_data = experiment$data
    )
    output$summary <- renderPrint({
      experiment_data_final <- facet_var_spec$experiment_data()
      facet_var <- facet_var_spec$sample_var()
      req(facet_var)
      facet_col <- SummarizedExperiment::colData(experiment_data_final)[[facet_var]]
      summary(facet_col)
    })
  })
}

my_app <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "sampleVarSpec example",
        server = server,
        ui = ui,
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
