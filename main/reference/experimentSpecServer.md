# Module Server for Experiment Specification

This defines the server part for the experiment specification.

## Usage

``` r
experimentSpecServer(
  id,
  data,
  filter_panel_api,
  mae_name,
  name_annotation = "symbol",
  sample_vars_as_factors = TRUE,
  with_mae_col_data = TRUE
)
```

## Arguments

- id:

  (`string`) the shiny module id.

- data:

  (`reactive`)  
  `reactive(<teal_data>)` holding all the data sets provided during app
  initialization after going through the filters.

- filter_panel_api:

  (`FilterPanelAPI`)  
  object describing the actual filter panel API.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- name_annotation:

  (`string` or `NULL`)  
  which annotation column to use as name to return in the `genes` data.
  If `NULL`, then the `name` column will be set to empty strings.

- sample_vars_as_factors:

  (`flag`)  
  whether to convert the sample variables (columns in `colData()` of the
  experiment) from character to factor variables.

- with_mae_col_data:

  (`flag`)  
  whether to include the `colData()` of the MAE into the experiment
  `colData()`.

## Value

List with the following reactive objects:

- `data`: the
  [`hermes::AnyHermesData`](https://insightsengineering.github.io/hermes/latest-tag/reference/HermesData-class.html)
  experiment.

- `name`: the name of the experiment as selected by the user.

- `genes`: a `data.frame` with the genes in `data`, with columns `id`
  and `name`.

- `assays`: the names of the assays in `data`.

## See also

[`experimentSpecInput()`](https://insightsengineering.github.io/teal.modules.hermes/reference/experimentSpecInput.md)
for the module UI.

## Examples

``` r
ui <- function(id,
               mae_name) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = uiOutput(ns("encoding_ui")),
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
    output$encoding_ui <- renderUI({
      tags$div(
        experimentSpecInput(
          session$ns("my_experiment"),
          data,
          mae_name,
          label_experiments = "Please choose experiment"
        ),
        selectInput(
          session$ns("property"),
          "Please choose property",
          c("data", "name", "genes", "assays")
        )
      )
    })
    experiment <- experimentSpecServer(
      "my_experiment",
      data,
      filter_panel_api,
      mae_name
    )
    result <- reactive({
      req(input$property)
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
  data <- teal_data(MAE = hermes::multi_assay_experiment)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "experimentSpec example",
        server = server,
        server_args = list(mae_name = "MAE"),
        ui = ui,
        ui_args = list(mae_name = "MAE"),
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
if (interactive()) {
  my_app()
}
```
