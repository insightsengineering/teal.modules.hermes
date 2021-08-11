#' Teal Module for RNA-seq Barplot
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
                         post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_barplot,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_barplot,
    ui_args = list(
      mae_name = mae_name,
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
      selectInput(ns("assay_name"), "Select assay", choices = ""),
      optionalSelectInput(ns("facet_var"), "Optional facet variable"),
      optionalSelectInput(ns("fill_var"), "Optional fill variable"),
      selectizeInput(ns("x_var"), "Select x gene", choices = ""),
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
                          mae_name) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the call that creates the chosen experiment data object.
  experiment_call <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    dat <- datasets$get_filtered_datasets(mae_name)
    dat$get_filter_states(input$experiment_name)$get_call()
  })

  # When the chosen experiment call changes, we recompute gene names.
  genes <- eventReactive(experiment_call(), ignoreNULL = FALSE, {
    object <- experiment_data()
    rownames(object)
  })

  # When the chosen experiment changes, recompute the assay names.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the chosen experiment changes, recompute the colData variables.
  col_data_vars <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    names(SummarizedExperiment::colData(object))
  })

  # When the assay names change, update the choices for assay.
  observeEvent(assay_names(), {
    assay_name_choices <- assay_names()

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices,
      selected = assay_name_choices[1]
    )
  })

  # When the colData variables change, update the choices for facet_var and fill_var.
  observeEvent(col_data_vars(), {
    facet_fill_var_choices <- col_data_vars()

    id_names <- c("facet_var", "fill_var")
    for (i in seq_along(id_names)) {
      updateOptionalSelectInput(
        session,
        id_names[i],
        choices = facet_fill_var_choices,
        selected = character()
      )
    }
  })

  # When the genes are recomputed, update the choices for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    updateSelectizeInput(
      session,
      "x_var",
      choices = gene_choices,
      selected = gene_choices[1],
      server = TRUE
    )
  })

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- experiment_data()
    x_var <- input$x_var
    facet_var <- input$facet_var
    fill_var <- input$fill_var
    percentiles <- input$percentiles
    assay_name <- input$assay_name

    # Require which states need to be truthy.
    req(
      x_var,
      assay_name,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
      isTRUE(x_var %in% rownames(experiment_data)),
      isTRUE(all(c(facet_var, fill_var) %in% names(SummarizedExperiment::colData(experiment_data)))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))
    validate(need(
      percentiles[1] != percentiles[2],
      "please select two different quantiles - if you want only 2 groups, choose one quantile as 0 or 1"
    ))

    hermes::draw_barplot(
      object = experiment_data,
      assay_name = assay_name,
      x_var = x_var,
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
