library(teal.modules.hermes)

ui <- function(id,
               datasets) {
  ns <- NS(id)
  mae <- datasets$get_data("MAE", filtered = FALSE)
  experiment_name_choices <- names(mae)
  teal.devel::standard_layout(
    encoding = div(
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
      sampleVarSpecInput(ns("facet_var"), "Select variable")
    ),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(input,
                   output,
                   session,
                   datasets) {
  experiment_data <- reactive({
    req(input$experiment_name)
    mae <- datasets$get_data("MAE", filtered = TRUE)
    object <- mae[[input$experiment_name]]
    SummarizedExperiment::colData(object) <- hermes::df_cols_to_factor(SummarizedExperiment::colData(object))
    object
  })
  facet_var_spec <- sampleVarSpecServer(
    "facet_var",
    experiment_name = reactive({input$experiment_name}), # nolint
    original_data = experiment_data
  )
  output$summary <- renderPrint({
    experiment_data_final <- facet_var_spec$experiment_data()
    facet_var <- facet_var_spec$sample_var()
    req(facet_var)
    facet_col <- SummarizedExperiment::colData(experiment_data_final)[[facet_var]]
    summary(facet_col)
  })
}

my_app <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
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
