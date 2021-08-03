#' Teal Module for RNA-seq Quality Control
#'
#' This module conducts quality control on a SummarizedExperiment input
#' for RNA-seq gene expression analysis.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' library(hermes)
#' utils::data("multi_assay_experiment", package = "hermes")
#' for (i in seq_along(multi_assay_experiment)) {
#'   multi_assay_experiment[[i]] <- hermes::HermesData(multi_assay_experiment[[i]])
#' }
#' mae_data <- dataset("MAE", multi_assay_experiment)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_quality(
#'         label = "Quality Control",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_quality <- function(label,
                         mae_name,
                         pre_output = NULL,
                         post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_quality,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_quality,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_quality sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_quality <- function(id,
                         datasets,
                         mae_name,
                         pre_output,
                         post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select Experiment", experiment_name_choices),
      sliderInput(ns("min_cpm"), label = ("Minimum CPM"), min = 1, max = 10, value = 5),
      sliderInput(ns("min_cpm_prop"), label = ("Minimum CPM Proportion"), min = 0.01, max = 0.99, value = 0.25),
      sliderInput(ns("min_corr"), label = ("Minimum Correlation"), min = 0.01, max = 0.99, value = 0.5),
      radioButtons(ns("min_depth"), label = ("Minimum Depth"), choices = c("Default", "Specify"), selected = NULL),
      conditionalPanel(condition = paste0("input['", ns("min_depth"), "']" , ".includes('Specify')"), sliderInput(ns("min_depth_continuous"), label = NULL, min = 1, max = 10, value = 5)),
      checkboxGroupInput(ns("filter"), label = ("Filter"), choiceNames = list("Genes", "Samples"), choiceValues = list("genes", "samples"), selected = NULL),
      optionalSelectInput(ns("annotate"), label = "Annotations", choices = "", selected = NULL)
    ),
    output = verbatimTextOutput(ns("quality")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_quality sets up the server with reactive graphs.
#' @inheritParams module_arguments
#' @export
srv_g_quality <- function(input,
                          output,
                          session,
                          datasets,
                          mae_name) {

  # Reactive function for experiment data since it is used in multiple places below.
  experiment_data <- reactive({
    req(input$experiment_name)

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the chosen experiment changes, recompute the annotations available.
  annotations <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    names(SummarizedExperiment::rowData(object))
  })

  # When the chosen experiment changes, recompute the maximum CPM available.
  max_cpm <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    object <- HermesData(object)
    max(edgeR::cpm(hermes::counts(object)))
  })

  observeEvent(annotations(), {
    # First: resolve all reactivity.
    annotations <- annotations()

    # Second: do the action.
    updateOptionalSelectInput(
      session,
      "annotate",
      choices = annotations,
      selected = annotations[1]
    )
  })

  observeEvent(input$experiment_name, {
    max_cpm <- max_cpm()

    updateSliderInput(
      session,
      "min_cpm",
      min = 1,
      max = max_cpm,
      value = 1
    )
  })

  output$quality <- renderPrint({
    object <- experiment_data()
    min_cpm <- input$min_cpm
    min_cpm_prop <- input$min_cpm_prop
    min_corr <- input$min_corr
    min_depth <- input$min_depth
    min_depth_continuous <- input$min_depth_continuous
    filter <- input$filter
    annotations <- input$annotate

    min_depth <- if (min_depth == "Specify") {
      min_depth_continuous
    } else {
      NULL
    }

    hermes::run_qc(
      object,
      min_cpm = min_cpm,
      min_cpm_prop = min_cpm_prop,
      min_corr = min_corr,
      min_depth = min_depth,
      filter = filter,
      annotations = annotations
    )
  })
}

#' @describeIn tm_g_quality sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_quality()
#' }
sample_tm_g_quality <- function() {
  mae <- hermes::multi_assay_experiment
  for (i in seq_along(mae)) {
    mae[[i]] <- hermes::HermesData(mae[[i]])
  }
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_quality(
          label = "Quality Control",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
