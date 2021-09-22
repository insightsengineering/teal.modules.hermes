#' Teal Module for RNA-seq Barplot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive barplot for RNA-seq gene expression
#' analysis. The percentiles are calculated at initialization.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' mae <- hermes::multi_assay_experiment
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_barplot(
#'         label = "barplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_barplot <- function(label,
                         exclude_assays = character(),
                         mae_name,
                         pre_output = NULL,
                         post_output = NULL,
                         summary_funs = list(
                               Mean = colMeans,
                               Median = matrixStats::colMedians,
                               Max = matrixStats::colMaxs
                             )) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_barplot,
    server_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_barplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_barplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_barplot <- function(id,
                         datasets,
                         mae_name,
                         summary_funs,
                         pre_output,
                         post_output) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay"), "Select assay"),
      sampleVarSpecInput(ns("facet_var"), "Facet variable"),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select x Gene(s)"),
      sliderInput(
        ns("percentiles"),
        "Select quantiles to be displayed",
        min = 0,
        max = 1,
        value = c(0.2, 0.8)
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("color_var"), "Optional fill variable", "Select function")
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_barplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_barplot <- function(input,
                          output,
                          session,
                          datasets,
                          mae_name,
                          exclude_assays = character(),
                          summary_funs) {

  experimentx <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name
  )

  assayx <- assaySpecServer(
    "assay",
    assays = experimentx$assays,
    exclude_assays = exclude_assays
  )

  sample_var_specs_x <- sampleVarSpecServer(
    "facet_var",
    experiment_name = experimentx$name,
    original_data = experimentx$data
  )

  color_var_specs_x <- sampleVarSpecServer(
    "color_var",
    experiment_name = experimentx$name,
    original_data = experimentx$data
  )

  gene_x <- geneSpecServer(
    "x_spec",
    summary_funs,
    experimentx$genes)

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- experimentx$data()
    facet_var <- sample_var_specs_x$sample_var()
    fill_var <- color_var_specs_x$sample_var()
    percentiles <- input$percentiles
    assay_name <- assayx()
    x_spec <- gene_x()


    # Require which states need to be truthy.
    req(
      x_spec,
      assay_name,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
      isTRUE(all(c(facet_var, fill_var) %in% names(SummarizedExperiment::colData(experiment_data)))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))
    validate(need(
      percentiles[1] != percentiles[2],
      "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
    ))
    validate_gene_spec(x_spec, rownames(experiment_data))

    hermes::draw_barplot(
      object = experiment_data,
      assay_name = assay_name,
      x_spec = x_spec,
      facet_var = facet_var,
      fill_var = fill_var,
      percentiles = percentiles
      )
  })
}

#' @describeIn tm_g_barplot sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_barplot()
#' }
sample_tm_g_barplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_barplot(
          label = "barplot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
