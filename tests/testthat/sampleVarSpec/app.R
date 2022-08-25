library(teal.modules.hermes)

ui <- function(id,
               data) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), data, "MAE"),
      sampleVarSpecInput(ns("facet_var"), "Select variable")
    ),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(id,
                   data) {
  moduleServer(id, function(input, output, session) {
    experiment <- experimentSpecServer(
      "experiment",
      data,
      filter_panel_api,
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
  mae_data <- teal.data::dataset("MAE", mae)
  data <- teal.data::teal_data(mae_data)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "sampleVarSpec example",
        server = server,
        ui = ui,
        filters = "all"
      )
    )
  )
  runApp(app)
}

my_app()
