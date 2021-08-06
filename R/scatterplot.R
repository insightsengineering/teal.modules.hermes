#' Teal Module for RNA-seq Scatterplot
#'
#' This module provides an interactive scatterplot for RNA-seq gene expression
#' analysis.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' library(hermes)
#' mae <- hermes::multi_assay_experiment
#' for (i in seq_along(mae)) {
#'   mae[[i]] <- hermes::HermesData(mae[[i]])
#' }
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_scatterplot(
#'         label = "scatterplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                             mae_name,
                             exclude_assays = "counts",
                             pre_output = NULL,
                             post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_scatterplot,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_scatterplot,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_scatterplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_scatterplot <- function(id,
                             datasets,
                             mae_name,
                             pre_output,
                             post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)
  smooth_method_choices <- c(
    Linear = "lm",
    Loess = "loess",
    None = "none"
  )
  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
      selectInput(ns("assay_name"), "Select assay", choices = ""),
      optionalSelectInput(ns("color_var"), "Optional color variable"),
      optionalSelectInput(ns("facet_var"), "Optional facet variable"),
      selectizeInput(ns("x_var"), "Select x gene", choices = ""),
      selectizeInput(ns("y_var"), "Select y gene", choices = ""),
      selectInput(ns("smooth_method"), "Select smoother", smooth_method_choices)
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_scatterplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              mae_name,
                              exclude_assays) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the calls that subset the genes of the chosen experiment data object.
  experiment_subset_calls <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    filtered_mae <- datasets$get_filtered_datasets(mae_name)
    filter_states <- filtered_mae$get_filter_states(input$experiment_name)
    subset_queue <- filter_states$queue_get("subset")
    sapply(subset_queue, function(x) x$get_call())
  })

  # When the chosen gene subset changes, we recompute gene names.
  genes <- eventReactive(experiment_subset_calls(), ignoreNULL = FALSE, {
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
    assay_name_choices <- setdiff(
      assay_names(),
      exclude_assays
    )

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices
    )
  })

  # When the colData variables change, update the choices for facet_var and color_var.
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
  })

  # When the genes are recomputed, update the choices for genes in the UI.
  observeEvent(genes(), {
    gene_choices <- genes()

    id_names <- c("x_var", "y_var")
    for (i in seq_along(id_names)) {
      updateSelectizeInput(
        session,
        id_names[i],
        choices = gene_choices,
        selected = gene_choices[i],
        server = TRUE
      )
    }
  })

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- experiment_data()
    x_var <- input$x_var
    y_var <- input$y_var
    facet_var <- input$facet_var
    color_var <- input$color_var
    assay_name <- input$assay_name
    smooth_method <- input$smooth_method

    # Require which states need to be truthy.
    req(
      x_var,
      y_var,
      smooth_method,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      is_blank(assay_name) || isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
      isTRUE(all(c(x_var, y_var) %in% rownames(experiment_data))),
      isTRUE(all(c(facet_var, color_var) %in% names(SummarizedExperiment::colData(experiment_data)))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))
    validate(need(
      !is_blank(assay_name),
      "no assays are available for this experiment, please choose another experiment"
    ))
    validate(need(x_var != y_var, "please select different genes for x and y variables"))

    hermes::draw_scatterplot(
      object = experiment_data,
      assay_name = assay_name,
      x_var = x_var,
      y_var = y_var,
      facet_var = facet_var,
      color_var = color_var,
      smooth_method = smooth_method
    )
  })
}

#' @describeIn tm_g_scatterplot sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_scatterplot()
#' }
sample_tm_g_scatterplot <- function() {
  mae <- hermes::multi_assay_experiment
  for (i in seq_along(mae)) {
    this_experiment <- mae[[i]]
    this_experiment <- hermes::HermesData(this_experiment)
    this_experiment <- hermes::normalize(this_experiment)
    mae[[i]] <- this_experiment
  }
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_scatterplot(
          label = "scatterplot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
