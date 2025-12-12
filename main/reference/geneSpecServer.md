# Module Server for Gene Signature Specification

This defines the server part for the gene signature specification.

## Usage

``` r
geneSpecServer(
  id,
  funs,
  gene_choices,
  label_modal_title = "Enter list of genes",
  label_modal_footer = c("Please enter a comma-separated list of gene IDs and/or names.",
    "(Note that genes not included in current choices will be removed)")
)
```

## Arguments

- id:

  (`string`) the shiny module id.

- funs:

  (static named `list`)  
  names of this list will be used for the function selection drop down
  menu.

- gene_choices:

  (reactive `data.frame`)  
  returns the possible gene choices to populate in the UI, as a
  `data.frame` with columns `id` and `name`.

- label_modal_title:

  (`string`)  
  title for the dialog that asks for the text input.

- label_modal_footer:

  (`character`)  
  lines of text to use for the footer of the dialog.

## Value

Reactive
[`hermes::GeneSpec`](https://insightsengineering.github.io/hermes/latest-tag/reference/GeneSpec.html)
which can be used as input for the relevant `hermes` functions.

## See also

[`geneSpecInput()`](https://insightsengineering.github.io/teal.modules.hermes/reference/geneSpecInput.md)
for the module UI.

## Examples

``` r
ui <- function(id, funs) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      geneSpecInput(
        ns("my_genes"),
        funs = funs,
        label_funs = "Please select function"
      )
    ),
    output = textOutput(ns("result"))
  )
}
server <- function(id,
                   data,
                   funs) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    gene_choices <- reactive({
      mae <- data()[["MAE"]]
      object <- mae[[1]]
      gene_ids <- rownames(object)
      gene_names <- SummarizedExperiment::rowData(object)$symbol
      gene_data <- data.frame(
        id = gene_ids,
        name = gene_names
      )
      gene_data[order(gene_data$name), ]
    })
    gene_spec <- geneSpecServer(
      "my_genes",
      funs = funs,
      gene_choices = gene_choices
    )
    output$result <- renderText({
      validate_gene_spec(
        gene_spec(),
        gene_choices()$id
      )
      gene_spec <- gene_spec()
      gene_spec$get_label()
    })
  })
}
funs <- list(mean = colMeans)
my_app <- function() {
  data <- teal_data(MAE = hermes::multi_assay_experiment)
  app <- init(
    data = data,
    modules = modules(
      module(
        label = "GeneSpec example",
        server = server,
        server_args = list(funs = funs),
        ui = ui,
        ui_args = list(funs = funs),
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
