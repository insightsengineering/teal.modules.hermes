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
#'     tm_g_boxplot(
#'       label = "boxplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_boxplot <- function(label,
                         mae_name,
                         exclude_assays = character(),
                         summary_funs = list(
                           None = NULL,
                           Mean = colMeans,
                           Median = matrixStats::colMedians,
                           Max = matrixStats::colMaxs
                         ),
                         pre_output = NULL,
                         post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs, null.ok = TRUE)
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

  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      tags$label("Jitter"),
      shinyWidgets::switchInput(ns("jitter"), value = FALSE, size = "mini"),
      tags$label("Violin Plot"),
      shinyWidgets::switchInput(ns("violin"), value = FALSE, size = "mini"),
      teal.devel::panel_group(
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("strat"), "Optional stratifying variable"),
          sampleVarSpecInput(ns("color"), "Optional color variable"),
          sampleVarSpecInput(ns("facet"), "Optional facet variable")
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
  multi <- multiSampleVarSpecServer(
    inputIds = c("strat", "color", "facet"),
    experiment_name = experiment$name,
    original_data = experiment$data
  )
  genes <- geneSpecServer(
    "genes",
    funs = summary_funs,
    gene_choices = experiment$genes
  )
  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- multi$experiment_data()
    strat <- multi$vars$strat()
    genes <- genes()
    facet <- multi$vars$facet()
    color <- multi$vars$color()
    assay <- assay()
    jitter <- input$jitter
    violin <- input$violin

    req(
      assay,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay %in% SummarizedExperiment::assayNames(experiment_data)),
      is.null(facet) || isTRUE(facet %in% names(SummarizedExperiment::colData(experiment_data))),
      is.null(color) || isTRUE(color %in% names(SummarizedExperiment::colData(experiment_data))),
      is.null(strat) || isTRUE(strat %in% names(SummarizedExperiment::colData(experiment_data))),
      cancelOutput = FALSE
    )

    validate_gene_spec(genes, rownames(experiment_data))

    hermes::draw_boxplot(
      object = experiment_data,
      assay_name = assay,
      genes = genes,
      x_var = strat,
      facet_var = facet,
      color_var = color,
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
