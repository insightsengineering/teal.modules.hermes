#' Most Expressed Genes Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function plots the most expressed genes.
#'
#' @inheritParams function_arguments
#'
#' @return Plot to be displayed in the teal app.
#'
#' @export
#'
#' @examples
#' library(hermes)
#' object <- HermesData(summarized_experiment)
#' result <- top_gene_plot(object, assay_name = "counts")
top_gene_plot <- function(object, assay_name) {
  top_gene <- hermes::top_genes(
    object = object,
    assay_name = assay_name,
    summary_fun = rowMeans
  )
  hermes::autoplot(
    top_gene,
    x_lab = "Gene",
    y_lab = paste("Mean", assay_name, "across samples")
  )
}

#' Correlation Heatmap Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function plots the correlation heatmap.
#'
#' @inheritParams function_arguments
#'
#' @return Plot to be displayed in the teal app.
#'
#' @export
#'
#' @examples
#' library(hermes)
#' object <- HermesData(summarized_experiment)
#' result <- heatmap_plot(object, assay_name = "counts")
heatmap_plot <- function(object, assay_name) {
  heatmap <- hermes::correlate(
    object = object,
    assay_name = assay_name
  )
  hermes::autoplot(heatmap)
}

#' Teal Module for RNA-seq Quality Control
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module adds quality flags, filters by genes and/or samples,
#' normalizes `AnyHermesData` objects and provides interactive plots
#' for RNA-seq gene expression quality control.
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
#'     tm_g_quality(
#'       label = "Quality Control",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_quality <- function(label,
                         mae_name,
                         exclude_assays = character(),
                         pre_output = NULL,
                         post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_quality,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_quality,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = mae_name
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
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      selectInput(
        ns("plot_type"),
        "Plot Type",
        choices = c(
          "Histogram",
          "Q-Q Plot",
          "Density",
          "Boxplot",
          "Top Genes Plot",
          "Correlation Heatmap"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Top Genes Plot' || input.plot_type == 'Correlation Heatmap'",
        ns = ns,
        assaySpecInput(ns("assay"))
      ),
      tags$label("Gene Filter Settings", class = "text-primary"),
      shinyWidgets::switchInput(
        ns("filter_gene"),
        value = TRUE,
        size = "mini"
      ),
      conditionalPanel(
        condition = "input.filter_gene",
        ns = ns,
        sliderInput(ns("min_cpm"), label = ("Minimum CPM"), min = 1, max = 10, value = 5),
        sliderInput(ns("min_cpm_prop"), label = ("Minimum CPM Proportion"), min = 0.01, max = 0.99, value = 0.25),
        optionalSelectInput(
          ns("annotate"),
          label = "Required Annotations",
          choices = "",
          selected = "",
          multiple = TRUE
        )
      ),
      tags$label("Sample Filter Settings", class = "text-primary"),
      shinyWidgets::switchInput(
        ns("filter_sample"),
        value = TRUE,
        size = "mini"
      ),
      conditionalPanel(
        condition = "input.filter_sample",
        ns = ns,
        sliderInput(ns("min_corr"), label = ("Minimum Correlation"), min = 0.01, max = 0.99, value = 0.5),
        radioButtons(
          ns("min_depth"),
          label = "Minimum Depth",
          choices = c("Default", "Specify"),
          selected = "Default"
        ),
        conditionalPanel(
          condition = "input.min_depth == 'Specify'",
          ns = ns,
          sliderInput(ns("min_depth_continuous"), label = NULL, min = 1, max = 10, value = 1)
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_quality sets up the server with reactive graphs.
#' @inheritParams module_arguments
#' @export
srv_g_quality <- function(id,
                          datasets,
                          mae_name,
                          exclude_assays) {
  moduleServer(id, function(input, output, session) {
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

    experiment_properties <- eventReactive(experiment$name(), {
      data <- experiment$data()
      cpm <- edgeR::cpm(hermes::counts(data))
      depth <- colSums(hermes::counts(data))
      list(
        annotations = names(hermes::annotation(data)),
        min_cpm_calc = floor(min(cpm)),
        max_cpm_calc = floor(max(cpm)),
        min_depth_calc = min(depth),
        max_depth_calc = max(depth)
      )
    })

    observeEvent(experiment_properties(), {
      properties <- experiment_properties()

      updateOptionalSelectInput(
        session,
        "annotate",
        choices = properties$annotations,
        selected = "WidthBP"
      )
      updateSliderInput(
        session,
        "min_cpm",
        min = properties$min_cpm_calc,
        max = properties$max_cpm_calc,
        value = properties$min_cpm_calc
      )
      updateSliderInput(
        session,
        "min_depth_continuous",
        min = properties$min_depth_calc,
        max = properties$max_depth_calc,
        value = properties$min_depth_calc
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
      object <- experiment$data()

      already_added <- ("control_quality_flags" %in% names(hermes::metadata(object)))
      validate(need(!already_added, "Quality flags have already been added to this experiment"))
      if (any(c("cpm", "rpkm", "tpm", "voom", "vst") %in% SummarizedExperiment::assayNames(object))) {
        showNotification("Original normalized assays will be overwritten", type = "warning")
      }

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

      validate(need(
        nrow(result) >= 2,
        "Please change gene filters to ensure that there are at least 2 genes"
      ))

      hermes::normalize(result)
    })

    output$plot <- renderPlot({
      object_final <- object_final()
      plot_type <- input$plot_type
      assay_name <- assay()

      switch(plot_type,
        "Histogram" = hermes::draw_libsize_hist(object_final),
        "Density" = hermes::draw_libsize_densities(object_final),
        "Q-Q Plot" = hermes::draw_libsize_qq(object_final),
        "Boxplot" = hermes::draw_nonzero_boxplot(object_final),
        "Top Genes Plot" = top_gene_plot(object_final, assay_name = assay_name),
        "Correlation Heatmap" = heatmap_plot(object_final, assay_name = assay_name)
      )
    })
  })
}

#' @describeIn tm_g_quality sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_quality()
#' }
sample_tm_g_quality <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      tm_g_quality(
        label = "quality",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
