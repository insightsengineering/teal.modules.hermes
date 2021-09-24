#' Teal Module for RNA-seq Boxplot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive boxplot for RNA-seq gene expression
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
#'       tm_g_boxplot(
#'         label = "boxplot",
#'         mae_name = "MAE"
#'       )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_boxplot <- function(label,
                         mae_name,
                         exclude_assays = character(),
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
    server = srv_g_boxplot,
    server_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_boxplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_boxplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_boxplot <- function(id,
                         datasets,
                         mae_name,
                         summary_funs,
                         pre_output,
                         post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = TRUE)


  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select gene(s) of interest"),
      tags$label("Jitter"),
      shinyWidgets::switchInput(ns("jitter"), value = FALSE, size = "mini"),
      tags$label("Violin Plot"),
      shinyWidgets::switchInput(ns("violin"), value = FALSE, size = "mini"),
      teal.devel::panel_group(
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("x_var"), "Optional stratifying variable"),
          sampleVarSpecInput(ns("color_var"), "Optional color variable"),
          sampleVarSpecInput(ns("facet_var"), "Optional facet variable")
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_boxplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_boxplot <- function(input,
                          output,
                          session,
                          datasets,
                          mae_name,
                          exclude_assays,
                          summary_funs) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
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
    inputIds = c("x_var", "color_var", "facet_var"),
    experiment_name = experiment$name,
    original_data = experiment$data
  )

  # When the chosen experiment call changes, we recompute gene names.
  x_spec <- geneSpecServer("x_spec", summary_funs, experiment$genes)

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- sample_var_specs$experiment_data()
    x_var <- sample_var_specs$vars$x_var()
    genes <- x_spec()
    facet_var <- sample_var_specs$vars$facet_var()
    color_var <- sample_var_specs$vars$color_var()
    assay_name <- assay()
    jitter <- input$jitter
    violin <- input$violin

    validate_gene_spec(genes, rownames(experiment_data))

    # Require which states need to be truthy.
    genes_not_included <- setdiff(genes$get_genes(), rownames(experiment_data))
      req(
        genes$get_genes(),
        assay_name,
        # Note: The following statements are important to make sure the UI inputs have been updated.
        isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
        length(genes_not_included) == 0,
        is.null(facet_var) || isTRUE(facet_var %in% names(SummarizedExperiment::colData(experiment_data))),
        is.null(color_var) || isTRUE(color_var %in% names(SummarizedExperiment::colData(experiment_data))),
        is.null(x_var) || isTRUE(x_var %in% names(SummarizedExperiment::colData(experiment_data))),
        cancelOutput = FALSE
      )

     # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
      validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))

    #hermes::draw_boxplot(
    draw_boxplot(
      object = experiment_data,
      assay_name = assay_name,
      x_var = x_var,
      genes = genes,
      facet_var = facet_var,
      color_var = color_var,
      jitter = jitter,
      violin = violin
    )
  })
}

#' @describeIn tm_g_boxplot sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_boxplot()
#' }
sample_tm_g_boxplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
        tm_g_boxplot(
          label = "boxplot",
          mae_name = "MAE"
        )
    )
  )
  shinyApp(app$ui, app$server)
}
