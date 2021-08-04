# To do: add roxygen documentation here
qc_filter_normalize <- function(object,
                                min_cpm,
                                min_cpm_prop,
                                min_corr,
                                min_depth,
                                filter,
                                annotations) {
  assert_class(object, "AnyHermesData")

  control <- hermes::control_quality(
    min_cpm = min_cpm,
    min_cpm_prop = min_cpm_prop,
    min_corr = min_corr,
    min_depth = min_depth
  )

  result <- hermes::add_quality_flags(object, control = control)
  result <- hermes::filter(result, what = filter, annotation_required = annotations)
  result <- hermes::normalize(result)

  result
}

#' Teal Module for RNA-seq Quality Control
#'
#' This module conducts quality control for RNA-seq gene expression analysis.
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
      selectInput(ns("assayname"), "Select Assay", choices = ""),
      selectInput(ns("plot_type"), "Plot Type", choices = c("Histogram", "Q-Q Plot", "Density", "Boxplot", "Top Genes Plot", "Correlation Heatmap")),
      sliderInput(ns("min_cpm"), label = ("Minimum CPM"), min = 1, max = 10, value = 5),
      sliderInput(ns("min_cpm_prop"), label = ("Minimum CPM Proportion"), min = 0.01, max = 0.99, value = 0.25),
      sliderInput(ns("min_corr"), label = ("Minimum Correlation"), min = 0.01, max = 0.99, value = 0.5),
      radioButtons(ns("min_depth"), label = ("Minimum Depth"), choices = c("Default", "Specify"), selected = "Default"),
      conditionalPanel(condition = paste0("input['", ns("min_depth"), "']" , ".includes('Specify')"), sliderInput(ns("min_depth_continuous"), label = NULL, min = 1, max = 10, value = 1)),
      checkboxGroupInput(ns("filter"), label = ("Filter"), choices = list("Genes" = "genes", "Samples" = "samples"), selected = c("genes", "samples")),
      optionalSelectInput(ns("annotate"), label = "Required Annotations", choices = "", selected = "", multiple = TRUE)
    ),
    output = plotOutput(ns("quality")),
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

  # When the chosen experiment changes, recompute the assay names.
  assays <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the chosen experiment changes, recompute the maximum CPM available.
  max_cpm <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    floor(max(edgeR::cpm(hermes::counts(object))))
  })

  # When the chosen experiment changes, recompute the maximum library size (depth) available.
  max_depth <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    max(colSums(hermes::counts(object)))
  })

  observeEvent(annotations(), {
    annotations <- annotations()

    updateOptionalSelectInput(
      session,
      "annotate",
      choices = annotations,
      selected = "WidthBP"
    )
  })

  observeEvent(input$experiment_name, {
    assays <- assays()

    updateSelectInput(
      session,
      "assayname",
      choices = assays,
      selected = assays[1]
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

  observeEvent(input$experiment_name, {
    max_depth <- max_depth()

    updateSliderInput(
      session,
      "min_depth_continuous",
      min = 1,
      max = max_depth,
      value = 1
    )
  })

  min_depth_final <- reactive({
    min_depth <- input$min_depth
    min_depth_continuous <- input$min_depth_continuous
    if (min_depth == "Specify") {
      req(min_depth_continuous)
      min_depth_continuous
    } else {
      NULL
    }
  })

  object_final <- reactive({
    object <- experiment_data()
    assays <- input$assayname
    min_cpm <- input$min_cpm
    min_cpm_prop <- input$min_cpm_prop
    min_corr <- input$min_corr
    min_depth_final <- min_depth_final()
    filter <- input$filter
    annotations <- input$annotate

    req(
      min_cpm,
      min_cpm_prop,
      min_corr,
      annotations
    )

    qc_filter_normalize(
      object,
      min_cpm = min_cpm,
      min_cpm_prop = min_cpm_prop,
      min_corr = min_corr,
      min_depth = min_depth_final,
      filter = filter,
      annotations = annotations
    )
  })

  output$quality <- renderPlot({
    object_final <- object_final()
    plot_type <- input$plot_type
    top_gene <- hermes::top_genes(object_final,
                                  n_top = 10,
                                  summary_fun = rowMeans)
    # heatmap <- hermes::correlate(object_final,
    #                              assay_name = input$assayname,
    #                              method = "spearman")

    switch(
      plot_type,
      "Histogram" = hermes::draw_libsize_hist(object_final),
      "Density" = hermes::draw_libsize_densities(object_final),
      "Q-Q Plot" = hermes::draw_libsize_qq(object_final),
      "Boxplot" = hermes::draw_nonzero_boxplot(object_final),
      "Top Genes Plot" = hermes::autoplot(top_gene)
      # "Correlation Heatmap" = hermes::autoplot(heatmap)
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
