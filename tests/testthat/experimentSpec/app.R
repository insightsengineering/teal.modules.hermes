library(teal.modules.hermes)

ui <- function(id,
               data,
               mae_name) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = div(
      experimentSpecInput(
        ns("my_experiment"),
        data,
        mae_name,
        label_experiments = "Please choose experiment"
      ),
      selectInput(
        ns("property"),
        "Please choose property",
        c("data", "name", "genes", "assays")
      )
    ),
    output = div(
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
  mae <- hermes::multi_assay_experiment
  mae_name <- "MAE"
  mae_data <- teal.data::dataset(mae_name, mae)
  data <- teal.data::teal_data(mae_data)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "experimentSpec example",
        server = server,
        server_args = list(mae_name = mae_name),
        ui = ui,
        ui_args = list(mae_name = mae_name),
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
