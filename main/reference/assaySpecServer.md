# Module Server for Assay Specification

This defines the server part for the assay specification.

## Usage

``` r
assaySpecServer(id, assays, exclude_assays = character())
```

## Arguments

- id:

  (`string`) the shiny module id.

- assays:

  (reactive `character`)  
  available assays in the currently selected experiment.

- exclude_assays:

  (`character`)  
  names of the assays which should not be included in choices in the
  teal module.

## Value

The chosen assay as a reactive string.

## See also

[`assaySpecInput()`](https://insightsengineering.github.io/teal.modules.hermes/reference/assaySpecInput.md)
for the module UI.

## Examples

``` r
ui <- function(id) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = uiOutput(ns("encoding_ui")),
    output = textOutput(ns("result"))
  )
}

server <- function(id, data, filter_panel_api) {
  moduleServer(id, module = function(input, output, session) {
    output$encoding_ui <- renderUI({
      tags$div(
        experimentSpecInput(session$ns("experiment"), data, "MAE"),
        assaySpecInput(
          session$ns("assay"),
          label_assays = "Please choose assay"
        )
      )
    })
    experiment <- experimentSpecServer(
      id = "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
      mae_name = "MAE"
    )
    assay <- assaySpecServer(
      "assay",
      experiment$assays,
      exclude_assays = c("counts", "cpm", "tpm", "bla")
    )
    output$result <- renderPrint({
      assay()
    })
  })
}

my_app <- function() {
  data <- teal_data(MAE = hermes::multi_assay_experiment)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "assaySpec example",
        server = server,
        ui = ui,
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
