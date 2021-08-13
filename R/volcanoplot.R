#' Teal Module for RNA-seq Volcano Plot
#'
#' This module provides an interactive volcano plot for RNA-seq gene expression
#' analysis.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' library(DT)
#' library(hermes)
#' library(teal)
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
#'       tm_g_volcanoplot(
#'         label = "volcanoplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_volcanoplot <- function(label,
                             mae_name,
                             pre_output = NULL,
                             post_output = NULL) {

  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_volcanoplot,
    server_args = list(mae_name = mae_name),
    ui = ui_g_volcanoplot,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}

#' @describeIn tm_g_volcanoplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_volcanoplot <- function(id, datasets, mae_name, pre_output, post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = TRUE)
  object <- hermes::HermesData(mae[[1]])
  colData(object) <- df_char_to_factor(colData(object))
  factor_2L <- names(colData(object))[apply(colData(object),2,function(s)nlevels(factor(s))) == 2L]

  teal.devel::standard_layout(
    output = div(
      plotOutput(ns("plot")),
      DTOutput(ns("table"))),
    pre_output = pre_output,
    post_output = post_output,
    encoding = div(
      selectInput(ns("experiment_name"), "Select experiment", names(mae)),
      selectInput(ns("compare_groups"), "Compare Groups", choices = factor_2L ),
      selectInput(ns("method"), "Method", choices = c("voom", "deseq2") ),
      sliderInput(ns("log2_fc_thresh"), "Log2 fold change threshold", value = 2.5, min = 0.1, max = 10),
      sliderInput(ns("adj_p_val_thresh"), "Adjusted p-value threshold", value = 0.05, min = 0.001, max = 1),
      tags$label("Show Top Differentiated Genes"),
      shinyWidgets::switchInput(ns("show_top_gene"), value = FALSE, size = "mini")
    )
  )
}

#' @describeIn tm_g_volcanoplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_volcanoplot <- function(input, output, session, datasets, mae_name) {

  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)

    mae <- datasets$get_data(mae_name, filtered = TRUE)

    object <- hermes::HermesData(mae[[input$experiment_name]])
    colData(object) <- df_char_to_factor(colData(object))
    object
  })

  # When the chosen experiment changes, recompute the colData variables.
  # We only select the colData variables with 2 levels
  col_data_vars <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    names(colData(object))[apply(colData(object),2,function(s)nlevels(factor(s))) == 2L]
  })

  # When the colData variables change, update the choices for compare_groups.
  observeEvent(col_data_vars(), {
    facet_fill_var_choices <- col_data_vars()

    id_names <- c("compare_groups")
    for (i in seq_along(id_names)) {
      updateSelectInput(
        session,
        id_names[i],
        choices = facet_fill_var_choices,
        selected = facet_fill_var_choices[1]
      )
    }
  })

  # When the filtered data set or the chosen experiment changes, update
  # the call that creates the Heremes object for differential expression.
  diff_expr <- reactive({
    object <- experiment_data()
    compare_groups <- input$compare_groups
    method <- input$method


    req(
      compare_groups,
        method,
      cancelOutput = FALSE
    )



    diff_expression(object, group = compare_groups, method = method)
  })




  output$plot <- renderPlot({
    # Resolve all reactivity.
    diff_expr_result <- diff_expr()
    log2_fc_thresh <- input$log2_fc_thresh
    adj_p_val_thresh <- input$adj_p_val_thresh
    compare_groups <- input$compare_groups

    # Require which states need to be truthy.
    req(
      log2_fc_thresh,
      adj_p_val_thresh,
      compare_groups
    )



    hermes::autoplot(diff_expr_result, adj_p_val_thresh = adj_p_val_thresh, log2_fc_thresh = log2_fc_thresh)
  })


  # display top genes if show_top_gene is TRUE
  show_top_gene_diffexpr <- reactive({
    if (input$show_top_gene) {
      diff_expr()
    }
  })

  output$table <- renderDT({

    DT::datatable(show_top_gene_diffexpr(),
                  rownames = TRUE,
                  options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
                  caption = "Top Differentiated Genes")
  })
}

#' @describeIn tm_g_volcanoplot sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#'  sample_tm_g_volcanoplot()
#' }
sample_tm_g_volcanoplot <- function() {
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
        tm_g_volcanoplot(
          label = "volcanoplot",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
