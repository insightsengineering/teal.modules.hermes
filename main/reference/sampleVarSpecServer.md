# Module Server for Sample Variable Specification

This defines the server part for the sample variable specification.

## Usage

``` r
sampleVarSpecServer(
  id,
  experiment_name,
  original_data,
  transformed_data = original_data,
  assign_lists = reactiveValues(),
  num_levels = NULL,
  categorical_only = !is.null(num_levels),
  explicit_na = FALSE,
  label_modal_title = "Please click to group the original factor levels"
)
```

## Arguments

- id:

  (`string`) the shiny module id.

- experiment_name:

  (reactive `string`)  
  name of the input experiment.

- original_data:

  (reactive `SummarizedExperiment`)  
  input experiment where the sample variables extracted via
  [`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  should be eligible for selection.

- transformed_data:

  (reactive `SummarizedExperiment`)  
  used when multiple sample variables can be selected in the app. In
  that case, pass here the pre-transformed data.

- assign_lists:

  (`reactivevalues`)  
  object to share factor level groupings across multiple sample
  variables.

- num_levels:

  (`count` or `NULL`)  
  required number of levels after combining original levels. If `NULL`
  then all numbers of levels are allowed.

- categorical_only:

  (`flag`)  
  whether only categorical variables should be selected from.

- explicit_na:

  (`flag`)  
  whether the `colData` of `original_data` will be transformed with
  [hermes::h_df_factors_with_explicit_na](https://insightsengineering.github.io/hermes/latest-tag/reference/h_df_factors_with_explicit_na.html)
  before further processing. That means also that `NA` will be made an
  explicit factor level and counted for `num_levels`.

- label_modal_title:

  (`string`)  
  title for the dialog that asks for the text input.

## Value

Reactive
[`SummarizedExperiment::SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
which can be used as input for the relevant `hermes` functions.

## Note

Only atomic columns (e.g. not `DataFrame` columns) of the `colData`
which are not completely missing (`NA`) will be shown for selection. If
`num_levels` is specified then only factor columns will be available.

## See also

[`sampleVarSpecInput()`](https://insightsengineering.github.io/teal.modules.hermes/reference/sampleVarSpecInput.md)
for the module UI.

## Examples

``` r
ui <- function(id) {
  checkmate::assert_class(data, "teal_data")
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = uiOutput(ns("encoding_ui")),
    output = plotOutput(ns("plot"))
  )
}
server <- function(id,
                   data) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    output$encoding_ui <- renderUI({
      mae <- data()[["MAE"]]
      experiment_name_choices <- names(mae)
      tags$div(
        selectInput(session$ns("experiment_name"), "Select experiment", experiment_name_choices),
        sampleVarSpecInput(session$ns("facet_var"), "Select faceting variable")
      )
    })
    experiment_data <- reactive({
      req(input$experiment_name)
      mae <- data()[["MAE"]]
      object <- mae[[input$experiment_name]]
      SummarizedExperiment::colData(object) <-
        hermes::df_cols_to_factor(SummarizedExperiment::colData(object))
      object
    })
    facet_var_spec <- sampleVarSpecServer(
      "facet_var",
      experiment_name = reactive({
        input$experiment_name
      }),
      original_data = experiment_data
    )
    output$plot <- renderPlot({
      experiment_data_final <- facet_var_spec$experiment_data()
      facet_var <- facet_var_spec$sample_var()
      hermes::draw_boxplot(
        experiment_data_final,
        assay_name = "counts",
        genes = hermes::gene_spec(hermes::genes(experiment_data_final)[1]),
        facet_var = facet_var
      )
    })
  })
}
my_app <- function() {
  data <- teal_data(MAE = hermes::multi_assay_experiment)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "sampleVarSpec example",
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
