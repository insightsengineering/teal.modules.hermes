#' Teal Module for PCA Analysis
#'
#' This module provides an interactive principal components plot and an
#' interactive heatmap with correlation of principal components with sample
#' variables.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#' @export
#'
#' @examples
#' library(hermes)
#' mae <- hermes::multi_assay_experiment
#' for (i in seq_along(multi_assay_experiment)) {
#'   multi_assay_experiment[[i]] <- hermes::HermesData(multi_assay_experiment[[i]])
#' }
#' mae_data <- dataset("MAE", multi_assay_experiment)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_pca(
#'         label = "PCA plot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_pca <- function(label,
                     mae_name,
                     pre_output = NULL,
                     post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_pca,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_pca,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_pca sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_pca <- function(id,
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
      selectizeInput(ns("x_var"), "PCA Plot: Select Principal Component to Plot on X-axis",
                     choices = ""),
      selectizeInput(ns("y_var"), "PCA Plot: Select Principal Component to Plot on Y-axis",
                     choices = ""),
      tags$label("PCA Plot: Show Variance Percentage"),
      shinyWidgets::switchInput(ns("var_pct"), value = TRUE, size = "mini"),
      tags$label("PCA Plot: Show Sample Label"),
      shinyWidgets::switchInput(ns("label"), value = TRUE, size = "mini"),
      tags$label("PCA Plot: Repel Sample Label"),
      shinyWidgets::switchInput(ns("label_repel"), value = TRUE, size = "mini"),
      tags$label("Correlation Plot: Cluster columns for correlation heatmap"),
      shinyWidgets::switchInput(ns("cluster_columns"), value = FALSE, size = "mini"),
      tags$label("Show corresponding matrix"),
      shinyWidgets::switchInput(ns("show_matrix"), value = FALSE, size = "mini")
    ),
    output = tagList(
      tabsetPanel(type = "tabs",
                  tabPanel(
                    title = "PCA",
                    column(
                      width = 12,
                      div(style = "height:20px;"),
                      h4("Principal Components Analysis"),
                      plotOutput(ns("plot_pca")),
                      br(), br(), br(),
                      DT::DTOutput(ns("table_pca"))
                    )
                  ),
                  tabPanel(
                    title = "PCA Correlation",
                    column(
                      width = 12,
                      div(style = "height:20px;"),
                      h4("Correlation of Principal Components with Sample Variables"),
                      plotOutput(ns("plot_cor")),
                      br(), br(), br(),
                      DT::DTOutput(ns("table_cor"))
                    )
                  )
      )
      #)
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_pca sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_pca <- function(input,
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

  # When the chosen experiment changes, recompute the assay names.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the assay_names changes, update the choices for assay.
  observeEvent(assay_names(), {
    assay_name_choices <- assay_names()

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices,
      selected = assay_name_choices[1]
    )
  })

  # When the chosen experiment changes, recompute the colData variables.
  col_data_vars <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    names(SummarizedExperiment::colData(object))
  })

  # When the colData variables change, update the choices for color_var.
  observeEvent(col_data_vars(), {
    color_var_choices <- col_data_vars()

    updateOptionalSelectInput(
      session,
      "color_var",
      choices = color_var_choices,
      selected = character()
    )
  })

  # When the chosen experiment or assay name changes, recompute the PC
  pca_result <- reactive({
    req(input$experiment_name)
    req(input$assay_name)
    object <- experiment_data()
    assay_name <- assay_names()
    hermes::calc_pca(object, assay_name)
  })

  # When experiment or assay name changes, update choices for PCs in x_var and y_var
  observeEvent(pca_result(), {
    pca_result_x <- pca_result()$x
    pc_choices <- colnames(as.data.frame(pca_result_x))

    id_names <- c("x_var", "y_var")
    for (i in seq_along(id_names)) {
      updateSelectizeInput(
        session,
        id_names[i],
        choices = pc_choices,
        selected = pc_choices[i]
      )
    }
  })

  # Compute correlatin of PC with sample variables
  cor_result <- reactive({
    hermes::correlate(pca_result(), experiment_data())
  })

  # Compute & display pca matrix table if show_matrix is TRUE
  show_matrix_pca <- reactive({
    if (input$show_matrix) {
      pca_result_x <- pca_result()$x
      as.data.frame(pca_result_x)
    }
  })
  output$table_pca <- DT::renderDT({
    DT::datatable(show_matrix_pca(),
                  rownames = TRUE,
                  options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
                  caption = "PCA Matrix")
  })

  # Compute & display correlation matrix if show_matrix is TRUE
  show_matrix_cor <- reactive({
    if (input$show_matrix) {
      cor_result_df <- as.data.frame(cor_result())
    }
  })
  output$table_cor <- DT::renderDT({
    DT::datatable(show_matrix_cor(),
                  rownames = TRUE,
                  options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
                  caption = "Correlation Matrix")
  })

  # Turn off label_repel when input$label is off
  observeEvent(input$label, {
    if (input$label == FALSE) {
      shinyWidgets::updateSwitchInput(
        session,
        inputId = "label_repel",
        value = FALSE
      )
    }
  })

  # Render plot pca output
  output$plot_pca <- renderPlot({
    # Resolve all reactivity.
    pca_result <- pca_result()
    x_var <- as.numeric(substring(input$x_var, 3))
    y_var <- as.numeric(substring(input$y_var, 3))
    data <- as.data.frame(SummarizedExperiment::colData(experiment_data()))
    color_var <- input$color_var
    assay_name <- input$assay_name
    var_pct <- input$var_pct
    label <- input$label
    label_repel <- input$label_repel

    # Require which states need to be truthy.
    req(
      assay_name,
      # Note: The following statements are important to make sure the UI inputs have been updated.
      isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data())),
      #isTRUE(color_var %in% names(SummarizedExperiment::colData(experiment_data))),
      cancelOutput = FALSE
    )

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data()), "please use HermesData() on input experiments"))
    validate(need(x_var != y_var, "please select two different principal components"))

    hermes::autoplot(
      object = pca_result,
      assay_name = assay_name,
      x = x_var,
      y = y_var,
      data = data,
      colour = color_var,
      variance_percentage = var_pct,
      label = label,
      label.repel = label_repel
    )
  })

  # render correlation heatmap
  output$plot_cor <- renderPlot({
    # Resolve all reactivity.
    cor_result <- cor_result()
    cluster_columns <- input$cluster_columns

    hermes::autoplot(
      object = cor_result,
      cluster_columns = cluster_columns
    )
  })

}

#' @describeIn tm_g_pca sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_pca()
#' }
sample_tm_g_pca <- function() {
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
        tm_g_pca(
          label = "pca plot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}

sample_tm_g_pca()
