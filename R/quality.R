# To do: roxygen
top_gene_plot <- function(object, assay_name) {
  top_gene <- hermes::top_genes(
    object = object,
    assay_name = assay_name
  )
  hermes::autoplot(top_gene)
}

# To do: roxygen
heatmap_plot <- function(object, assay_name) {
  heatmap <- hermes::correlate(
    object = object,
    assay_name = assay_name
  )
  hermes::autoplot(heatmap)
}

#' Teal Module for RNA-seq Quality Control
#'
#' This module adds quality flags, filters by genes and/or samples,
#' normalizes `AnyHermesData` objects and provides interactive plots
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
      selectInput(ns("plot_type"), "Plot Type", choices = c("Histogram", "Q-Q Plot", "Density", "Boxplot", "Top Genes Plot", "Correlation Heatmap")),
      conditionalPanel(
        condition = "input.plot_type == 'Top Genes Plot' || input.plot_type == 'Correlation Heatmap'",
        ns = ns,
        selectInput(ns("assay_name"), "Select Assay", choices = ""),
      ),
      checkboxGroupInput(ns("filter"), label = ("Filter"), choices = list("Genes" = "genes", "Samples" = "samples"), selected = c("genes", "samples")),
      conditionalPanel(
        condition = "input.filter.includes('genes')",
        ns = ns,
        sliderInput(ns("min_cpm"), label = ("Minimum CPM"), min = 1, max = 10, value = 5),
        sliderInput(ns("min_cpm_prop"), label = ("Minimum CPM Proportion"), min = 0.01, max = 0.99, value = 0.25),
        optionalSelectInput(ns("annotate"), label = "Required Annotations", choices = "", selected = "", multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.filter.includes('samples')",
        ns = ns,
        sliderInput(ns("min_corr"), label = ("Minimum Correlation"), min = 0.01, max = 0.99, value = 0.5),
        radioButtons(ns("min_depth"), label = ("Minimum Depth"), choices = c("Default", "Specify"), selected = "Default"),
        conditionalPanel(
          condition = "input.min_depth == 'Specify'",
          ns = ns,
          sliderInput(ns("min_depth_continuous"), label = NULL, min = 1, max = 10, value = 1)
        )
      )
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
    names(hermes::annotation(object))
  })

  # When the chosen experiment changes, recompute the assay names.
  assays <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the chosen experiment changes, recompute the minimum and maximum CPM available.
  mini_cpm <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    floor(min(edgeR::cpm(hermes::counts(object))))
  })

  max_cpm <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    floor(max(edgeR::cpm(hermes::counts(object))))
  })

  # When the chosen experiment changes, recompute the minimum and maximum library size (depth) available.
  mini_depth <- eventReactive(input$experiment_name, {
    object <- experiment_data()
    min(colSums(hermes::counts(object)))
  })

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
      "assay_name",
      choices = assays,
      selected = assays[1]
    )
  })

  observeEvent(input$experiment_name, {
    mini_cpm <- mini_cpm()
    max_cpm <- max_cpm()

    updateSliderInput(
      session,
      "min_cpm",
      min = mini_cpm,
      max = max_cpm,
      value = mini_cpm
    )
  })

  observeEvent(input$experiment_name, {
    mini_depth <- mini_depth()
    max_depth <- max_depth()

    updateSliderInput(
      session,
      "min_depth_continuous",
      min = mini_depth,
      max = max_depth,
      value = mini_depth
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

  control <- reactive({
    min_cpm <- input$min_cpm
    min_cpm_prop <- input$min_cpm_prop
    min_corr <- input$min_corr
    min_depth_final <- min_depth_final()

    req(
      min_cpm,
      min_cpm_prop,
      min_corr
    )

    hermes::control_quality(
      min_cpm = min_cpm,
      min_cpm_prop = min_cpm_prop,
      min_corr = min_corr,
      min_depth = min_depth_final
    )
  })

  object_flagged <- reactive({
    control <- control()
    object <- experiment_data()

    validate(need(hermes::is_hermes_data(object), "please use HermesData() first on experiments"))

    hermes::add_quality_flags(
      object,
      control = control
    )
  })

  object_final <- reactive({
    object_flagged <- object_flagged()
    filter <- input$filter
    annotate <- input$annotate

    req(!is_blank(annotate))

    result <- hermes::filter(
      object_flagged,
      what = filter,
      annotation_required = annotate
    )
    hermes::normalize(result)
  })

  output$quality <- renderPlot({
    object_final <- object_final()
    plot_type <- input$plot_type
    assay_name <- input$assay_name

    switch(
      plot_type,
      "Histogram" = hermes::draw_libsize_hist(object_final),
      "Density" = hermes::draw_libsize_densities(object_final),
      "Q-Q Plot" = hermes::draw_libsize_qq(object_final),
      "Boxplot" = hermes::draw_nonzero_boxplot(object_final),
      "Top Genes Plot" = top_gene_plot(object_final, assay_name = assay_name),
      "Correlation Heatmap" = heatmap_plot(object_final, assay_name = assay_name)
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
    mae[[i]] <- hermes::normalize(hermes::HermesData(mae[[i]]))
  }
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_quality(
          label = "quality",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
