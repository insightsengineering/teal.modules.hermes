#' Teal Module for RNA-seq Barplot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive barplot for RNA-seq gene expression
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
#'   modules = modules(
#'     tm_g_barplot(
#'       label = "barplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_barplot <- function(label,
                         mae_name,
                         exclude_assays = character(),
                         summary_funs = list(
                           Mean = colMeans,
                           Median = matrixStats::colMedians,
                           Max = matrixStats::colMaxs
                         ),
                         pre_output = NULL,
                         post_output = NULL) {
  logger::log_info("Initializing tm_g_barplot")
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_barplot,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays,
      summary_funs = summary_funs
    ),
    ui = ui_g_barplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = mae_name
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
  teal.widgets::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      sampleVarSpecInput(ns("facet"), "Select Facet Variable"),
      geneSpecInput(ns("x"), summary_funs),
      sliderInput(
        ns("percentiles"),
        "Select Quantiles",
        min = 0,
        max = 1,
        value = c(0.2, 0.8)
      ),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(
            ns("fill"),
            label_vars = "Optional Fill Variable"
          )
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
srv_g_barplot <- function(id,
                          datasets,
                          mae_name,
                          exclude_assays,
                          summary_funs) {
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
    multi <- multiSampleVarSpecServer(
      c("facet", "fill"),
      experiment_name = experiment$name,
      original_data = experiment$data
    )
    x <- geneSpecServer(
      "x",
      funs = summary_funs,
      gene_choices = experiment$genes
    )

    output$plot <- renderPlot({
      # Resolve all reactivity.
      experiment_data <- multi$experiment_data()
      facet_var <- multi$vars$facet()
      fill_var <- multi$vars$fill()
      percentiles <- input$percentiles
      assay <- assay()
      x <- x()

      # Require which states need to be truthy.
      req(
        assay,
        # Note: The following statements are important to make sure the UI inputs have been updated.
        isTRUE(assay %in% SummarizedExperiment::assayNames(experiment_data)),
        isTRUE(all(c(facet_var, fill_var) %in% names(SummarizedExperiment::colData(experiment_data)))),
        cancelOutput = FALSE
      )

      # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
      validate(need(
        percentiles[1] != percentiles[2],
        "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
      ))
      validate_gene_spec(x, rownames(experiment_data))

      hermes::draw_barplot(
        object = experiment_data,
        assay_name = assay,
        x_spec = x,
        facet_var = facet_var,
        fill_var = fill_var,
        percentiles = percentiles
      )
    })
  })
}

#' @describeIn tm_g_barplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_barplot()
#' }
sample_tm_g_barplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- teal.data::dataset("MAE", mae)
  data <- teal.data::teal_data(mae_data)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_barplot(
        label = "barplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
