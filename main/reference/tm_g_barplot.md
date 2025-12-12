# Teal Module for RNA-seq Barplot

This module provides an interactive barplot for RNA-seq gene expression
analysis.

## Usage

``` r
tm_g_barplot(
  label,
  mae_name,
  exclude_assays = character(),
  summary_funs = list(Mean = colMeans, Median = matrixStats::colMedians, Max =
    matrixStats::colMaxs),
  pre_output = NULL,
  post_output = NULL,
  .test = FALSE,
  transformators = list()
)

ui_g_barplot(
  id,
  mae_name,
  summary_funs,
  pre_output,
  post_output,
  .test = FALSE
)

srv_g_barplot(
  id,
  data,
  filter_panel_api,
  reporter,
  mae_name,
  exclude_assays,
  summary_funs,
  .test = FALSE
)

sample_tm_g_barplot(.test = FALSE)
```

## Arguments

- label:

  (`string`)  
  menu item label of the module in the teal app.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- exclude_assays:

  (`character`)  
  names of the assays which should not be included in choices in the
  teal module.

- summary_funs:

  (named `list` of functions or `NULL`)  
  functions which can be used in the the gene signatures. For modules
  that support also multiple genes without summary, `NULL` can be
  included to not summarize the genes but provide all of them.

- pre_output:

  (`shiny.tag` or `NULL`)  
  placed before the output to put the output into context (for example a
  title).

- post_output:

  (`shiny.tag` or `NULL`)  
  placed after the output to put the output into context (for example
  the [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements can be useful).

- .test:

  (`flag`)  
  whether to display the internal structure of the plot for testing
  purposes.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- id:

  (`string`) the shiny module id.

- data:

  (`reactive`)  
  `reactive(<teal_data>)` holding all the data sets provided during app
  initialization after going through the filters.

- filter_panel_api:

  (`FilterPanelAPI`)  
  object describing the actual filter panel API.

- reporter:

  (`Reporter`) object

## Value

Shiny module to be used in the teal app.

## Functions

- `ui_g_barplot()`: sets up the user interface.

- `srv_g_barplot()`: sets up the server with reactive graph.

- `sample_tm_g_barplot()`: sample module function.

## Examples

``` r
data <- teal_data(MAE = hermes::multi_assay_experiment)
app <- init(
  data = data,
  modules = modules(
    tm_g_barplot(
      label = "barplot",
      mae_name = "MAE"
    )
  )
)
#> Initializing tm_g_barplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Alternatively you can run the sample module with this function call:
if (interactive()) {
  sample_tm_g_barplot()
}
```
