library(teal.modules.hermes)

ui <- function(id,
               datasets,
               mae_name) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
      experimentSpecInput(
        ns("my_experiment"),
        datasets,
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

server <- function(input,
                   output,
                   session,
                   datasets,
                   mae_name) {
  experiment <- experimentSpecServer(
    "my_experiment",
    datasets,
    mae_name
  )
  result <- reactive({
    switch(
      input$property,
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
    head(result)
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
