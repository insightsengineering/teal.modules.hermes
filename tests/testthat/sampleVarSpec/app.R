library(teal.modules.hermes)

ui <- function(id) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      uiOutput(ns("experiment_ui")),
      sampleVarSpecInput(ns("facet_var"), "Select variable")
    ),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(id,
                   data,
                   filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    output$experiment_ui <- renderUI({
      experimentSpecInput(session$ns("experiment"), data, "MAE")
    })
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
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "sampleVarSpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
