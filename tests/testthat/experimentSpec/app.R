library(teal.modules.hermes)

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

my_app <- function() {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "experimentSpec example",
        server = server,
        server_args = list(mae_name = "MAE"),
        ui = ui,
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
