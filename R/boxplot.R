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
#'     static = {
#'       tm_g_boxplot(
#'         label = "boxplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_boxplot <- function(label,
                         mae_name,
                         pre_output = NULL,
                         post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_boxplot,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_boxplot,
    ui_args = list(
      mae_name = mae_name,
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
      optionalSelectInput(ns("color_var"), "Optional color variable"),
      optionalSelectInput(ns("facet_var"), "Optional facet variable"),
      selectizeInput(ns("y_var"), "Select gene of interest", choices = ""),
      selectizeInput(ns("x_var"), "Select stratifying variable", choices = ""),
      tags$label("Jitter"),
      shinyWidgets::switchInput(ns("jitter"), value = FALSE, size = "mini")
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

  # When the colData variables change, update the choices for facet_var, color_var and x_var.
  observeEvent(col_data_vars(), {
    facet_color_var_choices <- col_data_vars()

    id_names <- c("facet_var", "color_var")
    for (i in seq_along(id_names)) {
      updateOptionalSelectInput(
        session,
        id_names[i],
        choices = facet_color_var_choices,
        selected = character()
      )
    }

    id_names <- c("x_var")
      updateSelectizeInput(
        session,
        id_names,
        choices = facet_color_var_choices,
        selected = facet_color_var_choices[1],
        server = TRUE
      )
  })

  # When the genes are recomputed, update the choice for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    id_names <- c("y_var")
      updateSelectizeInput(
        session,
        id_names,
        choices = gene_choices,
        selected = gene_choices[1],
        server = TRUE
      )
  })

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- experiment_data()
    x_var <- input$x_var
    y_var <- input$y_var
    facet_var <- input$facet_var
    color_var <- input$color_var
    assay_name <- input$assay_name
    jitter <- input$jitter

    # Require which states need to be truthy.
    req(
      x_var,
      y_var,
      assay_name,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
      isTRUE(y_var %in% rownames(experiment_data)),
      isTRUE(all(c(facet_var, color_var, x_var) %in% names(SummarizedExperiment::colData(experiment_data)))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))

    hermes::draw_boxplot(
      object = experiment_data,
      assay_name = assay_name,
      x_var = x_var,
      y_var = y_var,
      facet_var = facet_var,
      color_var = color_var,
      jitter = jitter
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
      static = {
        tm_g_boxplot(
          label = "boxplot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
