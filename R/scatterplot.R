#' Teal Module for RNA-seq Scatterplot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive scatterplot for RNA-seq gene expression
#' analysis.
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
#'     tm_g_scatterplot(
#'       label = "scatterplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                             mae_name,
                             exclude_assays = "counts",
                             summary_funs = list(
                               Mean = colMeans,
                               Median = matrixStats::colMedians,
                               Max = matrixStats::colMaxs
                             ),
                             pre_output = NULL,
                             post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_scatterplot,
    server_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_scatterplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = mae_name
  )
}

#' @describeIn tm_g_scatterplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_scatterplot <- function(id,
                             datasets,
                             mae_name,
                             summary_funs,
                             pre_output,
                             post_output) {
  ns <- NS(id)

  smooth_method_choices <- c(
    Linear = "lm",
    Loess = "loess",
    None = "none"
  )

  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select x Gene(s)"),
      geneSpecInput(ns("y_spec"), summary_funs, label_genes = "Select y Gene(s)"),
      teal.devel::panel_group(
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("color_var"), "Optional color variable"),
          sampleVarSpecInput(ns("facet_var"), "Optional facet variable"),
          selectInput(ns("smooth_method"), "Select smoother", smooth_method_choices)
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_scatterplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              mae_name,
                              exclude_assays,
                              summary_funs) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name
  )
  assay <- assaySpecServer(
    "assay",
    assays = experiment$assays,
    exclude_assays = exclude_assays
  )
  sample_var_specs <- multiSampleVarSpecServer(
    inputIds = c("facet_var", "color_var"),
    experiment_name = experiment$name,
    original_data = experiment$data
  )
  x_spec <- geneSpecServer("x_spec", summary_funs, experiment$genes)
  y_spec <- geneSpecServer("y_spec", summary_funs, experiment$genes)

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- sample_var_specs$experiment_data()
    x_spec <- x_spec()
    y_spec <- y_spec()
    facet_var <- sample_var_specs$vars$facet_var()
    color_var <- sample_var_specs$vars$color_var()
    assay_name <- assay()
    smooth_method <- input$smooth_method

    validate_gene_spec(x_spec, rownames(experiment_data))
    validate_gene_spec(y_spec, rownames(experiment_data))

    # Require which states need to be truthy.
    req(
      smooth_method,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
      is.null(facet_var) || isTRUE(facet_var %in% names(SummarizedExperiment::colData(experiment_data))),
      is.null(color_var) || isTRUE(color_var %in% names(SummarizedExperiment::colData(experiment_data))),
      cancelOutput = FALSE
    )

    hermes::draw_scatterplot(
      object = experiment_data,
      assay_name = assay_name,
      x_spec = x_spec,
      y_spec = y_spec,
      facet_var = facet_var,
      color_var = color_var,
      smooth_method = smooth_method
    )
  })
}

#' @describeIn tm_g_scatterplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_scatterplot()
#' }
sample_tm_g_scatterplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}


