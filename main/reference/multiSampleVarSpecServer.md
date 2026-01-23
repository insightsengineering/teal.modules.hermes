# Module Server for Specification of Multiple Sample Variables

When multiple sample variables are used in a given module, then this
wrapper makes it much easier to specify in the server function.

## Usage

``` r
multiSampleVarSpecServer(inputIds, original_data, ...)
```

## Arguments

- inputIds:

  (`character`)  
  multiple input IDs corresponding to the different sample variables
  specified in the UI function.

- original_data:

  (reactive `SummarizedExperiment`)  
  input experiment where the sample variables extracted via
  [`SummarizedExperiment::colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  should be eligible for selection.

- ...:

  additional arguments as documented in
  [`sampleVarSpecServer()`](https://insightsengineering.github.io/teal.modules.hermes/reference/sampleVarSpecServer.md),
  namely the mandatory `experiment_name` and the optional
  `categorical_only`, `num_levels` and `label_modal_title`.
  `transformed_data` and `assign_lists` should not be specified as they
  are already specified internally here.

## Value

List with the final transformed `experiment_data` reactive and a list
`vars` which contains the selected sample variables as reactives under
their input ID.

## Examples

``` r
if (FALSE) { # \dontrun{
# In the server use:
sample_var_specs <- multiSampleVarSpecServer(
  inputIds = c("facet_var", "color_var"),
  experiment_name = reactive({
    input$experiment_name
  }),
  original_data = ori_data # Please update the <ori_data>
)
# Then can extract the transformed data and selected variables later:
experiment_data <- sample_var_specs$experiment_data()
facet_var <- sample_var_specs$vars$facet_var()
color_var <- sample_var_specs$vars$color_var()
} # }
```
