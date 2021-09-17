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
      summary_funs = summary_funs
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
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),

      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay"), "Select assay"),
      sampleVarSpecInput(ns("facet_var"), "Optional facet variable"),
      sampleVarSpecInput(ns("color_var"), "Optional fill variable"),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select x Gene(s)"),


      # will become geneSpecInput
      # selectizeInput(ns("x_var"), "Select x gene", choices = ""),
      sliderInput(
        ns("percentiles"),
        "Select quantiles to be displayed",
        min = 0,
        max = 1,
        value = c(0.2, 0.8)
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

  experimentx <- experimentSpecServer(# return hermes data
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


  # When the genes are recomputed, update the choices for genes in the UI.
  # observeEvent(experimentx$genes(), {
  #   gene_choices <- unlist(experimentx$genes()$id)
  #
  #   updateSelectizeInput(
  #     session,
  #     "x_var",
  #     choices = gene_choices,
  #     selected = gene_choices[1],
  #     server = TRUE
  #   )
  # })

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- experimentx$data()

    x_var <- gene_x()

    facet_var <- sample_var_specs_x$sample_var()
    fill_var <- color_var_specs_x$sample_var()
    percentiles <- input$percentiles
    assay_name <- assayx() # input$assay_name


    assay_matrix <- assay(experiment_data, assay_name)

    x = as.vector(x_var$get_genes())

    # Require which states need to be truthy.
    req(
      x_var,
      assay_name,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experimentx$data())),
      #isTRUE(x_var %in% rownames(experimentx$data())),
      isTRUE(all(c(facet_var, fill_var) %in% names(SummarizedExperiment::colData(experimentx$data())))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experimentx$data()), "please use HermesData() on input experiments"))
    validate(need(
      percentiles[1] != percentiles[2],
      "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
    ))


      hermes::draw_barplot(
      object = experimentx$data(),
      assay_name = assay_name,
      x_var = x[1],
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

