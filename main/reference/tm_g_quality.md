# Teal Module for RNA-seq Quality Control

This module adds quality flags, filters by genes and/or samples,
normalizes `AnyHermesData` objects and provides interactive plots for
RNA-seq gene expression quality control.

## Usage

``` r
tm_g_quality(
  label,
  mae_name,
  exclude_assays = character(),
  pre_output = NULL,
  post_output = NULL,
  .test = FALSE,
  transformators = list()
)

ui_g_quality(id, mae_name, pre_output, post_output, .test = FALSE)

srv_g_quality(
  id,
  data,
  filter_panel_api,
  reporter,
  mae_name,
  exclude_assays,
  .test = FALSE
)

sample_tm_g_quality(.test = FALSE)
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

- `ui_g_quality()`: sets up the user interface.

- `srv_g_quality()`: sets up the server with reactive graphs.

- `sample_tm_g_quality()`: sample module function.

## Examples

``` r
data <- teal_data(MAE = hermes::multi_assay_experiment)
app <- init(
  data = data,
  modules = modules(
    tm_g_quality(
      label = "Quality Control",
      mae_name = "MAE"
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Alternatively you can run the sample module with this function call:
if (interactive()) {
  sample_tm_g_quality()
}
```
