# Teal Module for Survival Forest Plot

This module provides an interactive survival forest plot.

## Usage

``` r
tm_g_forest_tte(
  label,
  adtte_name,
  mae_name,
  adtte_vars = list(aval = "AVAL", is_event = "is_event", paramcd = "PARAMCD", usubjid =
    "USUBJID", avalu = "AVALU"),
  exclude_assays = "counts",
  summary_funs = list(Mean = colMeans, Median = matrixStats::colMedians, Max =
    matrixStats::colMaxs),
  pre_output = NULL,
  post_output = NULL,
  plot_height = c(600L, 200L, 2000L),
  plot_width = c(1360L, 500L, 2000L),
  .test = FALSE,
  transformators = list()
)

ui_g_forest_tte(
  id,
  adtte_name,
  mae_name,
  summary_funs,
  pre_output,
  post_output,
  .test = FALSE
)

srv_g_forest_tte(
  id,
  data,
  filter_panel_api,
  reporter,
  adtte_name,
  mae_name,
  adtte_vars,
  exclude_assays,
  summary_funs,
  plot_height,
  plot_width,
  .test = FALSE
)

sample_tm_g_forest_tte(.test = FALSE)
```

## Arguments

- label:

  (`string`)  
  menu item label of the module in the teal app.

- adtte_name:

  (`string`)  
  name of the `ADTTE` dataset.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- adtte_vars:

  (named `list` of `string`)  
  names of the variables to use in the `ADTTE` dataset. It should
  comprise elements:

  - `aval`: the numeric time-to-event variable.

  - `avalu`: the variable holding the unit of `aval`.

  - `is_event`: the logical event variable. It needs to be `TRUE` when
    there was an observed event, and `FALSE` if the time is censored
    without observed event.

  - `paramcd`: the character or factor parameter code variable, defining
    the type of time-to-event for selection in the module.

  - `usubjid`: the subject ID variable.

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

- plot_height:

  (`list`)  
  list of integers to set the default, minimum, and maximum plot height.

- plot_width:

  (`list`)  
  list of integers to set the default, minimum, and maximum plot width.

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

- `ui_g_forest_tte()`: sets up the user interface.

- `srv_g_forest_tte()`: sets up the server with reactive graph.

- `sample_tm_g_forest_tte()`: sample module function.

## Examples

``` r
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADTTE <- teal.data::rADTTE %>%
    dplyr::mutate(is_event = .data$CNSR == 0)
  MAE <- hermes::multi_assay_experiment
})
join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

app <- init(
  data = data,
  modules = modules(
    tm_g_forest_tte(
      label = "forestplot",
      adtte_name = "ADTTE",
      mae_name = "MAE"
    )
  )
)
#> Initializing tm_g_forest_tte
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Alternatively you can run the sample module with this function call:
if (interactive()) {
  sample_tm_g_forest_tte()
}
```
