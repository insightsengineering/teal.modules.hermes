# Module Server for `ADTTE` Specification

This defines the server part for the `ADTTE` specification. The
resulting data set `binned_adtte_subset` contains the subset of `ADTTE`
selected by the time-to-event endpoint, joined together with the gene
information extracted from specified assay and experiment, as numeric
and factor columns. The factor column is created by binning the numeric
column according to the quantile cutoffs specified in `probs`.

## Usage

``` r
adtteSpecServer(
  id,
  data,
  mae_name,
  adtte_name,
  adtte_vars,
  experiment_data,
  experiment_name,
  assay,
  genes,
  probs
)
```

## Arguments

- id:

  (`string`) the shiny module id.

- data:

  (`reactive`)  
  `reactive(<teal_data>)` holding all the data sets provided during app
  initialization after going through the filters.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- adtte_name:

  (`string`)  
  name of the `ADTTE` dataset.

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

- experiment_data:

  (reactive `AnyHermesData`)  
  input experiment.

- experiment_name:

  (reactive `string`)  
  name of the input experiment.

- assay:

  (reactive `string`)  
  name of the assay.

- genes:

  (reactive `GeneSpec`)  
  gene specification.

- probs:

  (reactive `numeric`)  
  probabilities to bin the gene or gene signature into.

## Value

List with the following elements:

- `binned_adtte_subset`: reactive containing the joined `ADTTE` and gene
  data.

- `gene_col`: reactive containing the string with the column name of the
  original numeric gene variable.

- `gene_factor`: string with the variable name for the binned gene data.

- `time_unit`: reactive string with the time unit for the current
  subset.

## See also

[`adtteSpecInput()`](https://insightsengineering.github.io/teal.modules.hermes/reference/adtteSpecInput.md)
for the module UI.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

ui <- function(id) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = uiOutput(ns("encoding_ui")),
    output = verbatimTextOutput(ns("summary"))
  )
}

server <- function(id, data, filter_panel_api) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    output$encoding_ui <- renderUI({
      tags$div(
        experimentSpecInput(session$ns("experiment"), data, mae_name = "MAE"),
        assaySpecInput(session$ns("assay")),
        geneSpecInput(session$ns("genes"), funs = list(Mean = colMeans)),
        adtteSpecInput(session$ns("adtte"))
      )
    })
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
      mae_name = "MAE"
    )
    assay <- assaySpecServer(
      "assay",
      assays = experiment$assays
    )
    genes <- geneSpecServer(
      "genes",
      funs = list(Mean = colMeans),
      gene_choices = experiment$genes
    )
    adtte <- adtteSpecServer(
      "adtte",
      data = data,
      adtte_name = "ADTTE",
      mae_name = "MAE",
      adtte_vars = list(
        aval = "AVAL",
        avalu = "AVALU",
        is_event = "is_event",
        paramcd = "PARAMCD",
        usubjid = "USUBJID"
      ),
      experiment_data = experiment$data,
      experiment_name = experiment$name,
      assay = assay,
      genes = genes,
      probs = reactive({
        0.5
      })
    )
    output$summary <- renderPrint({
      binned_adtte_subset <- adtte$binned_adtte_subset()
      summary(binned_adtte_subset)
    })
  })
}

my_app <- function() {
  data <- teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADTTE <- teal.data::rADTTE %>%
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment
  })
  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE", "MAE")]

  app <- init(
    data = data,
    modules = modules(
      module(
        label = "adtteSpec example",
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
