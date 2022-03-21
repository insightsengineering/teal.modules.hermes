#' Teal Module for RNA-seq Volcano Plot
#'
#' @description `r lifecycle::badge("experimental")`
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
#' mae <- hermes::multi_assay_experiment
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_volcanoplot(
#'       label = "volcanoplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_volcanoplot <- function(label,
                             mae_name,
                             exclude_assays = character(),
                             pre_output = NULL,
                             post_output = NULL) {
  logger::log_info("Initializing tm_g_volcanoplot")
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  teal::module(
    label = label,
    server = srv_g_volcanoplot,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_volcanoplot,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = mae_name
  )
}

#' @describeIn tm_g_volcanoplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_volcanoplot <- function(id,
                             datasets,
                             mae_name,
                             pre_output,
                             post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = TRUE)

  teal.widgets::standard_layout(
    output = div(
      plotOutput(ns("plot")),
      DT::DTOutput(ns("table"))
    ),
    pre_output = pre_output,
    post_output = post_output,
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      sampleVarSpecInput(ns("compare_group"), "Compare Groups", "Please group here into 2 levels"),
      tags$label("Show Top Differentiated Genes"),
      shinyWidgets::switchInput(ns("show_top_gene"), value = FALSE, size = "mini"),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          selectInput(ns("method"), "Method", choices = c("voom", "deseq2")),
          sliderInput(ns("log2_fc_thresh"), "Log2 fold change threshold", value = 2.5, min = 0.1, max = 10),
          sliderInput(ns("adj_p_val_thresh"), "Adjusted p-value threshold", value = 0.05, min = 0.001, max = 1)
        )
      )
    )
  )
}

#' @describeIn tm_g_volcanoplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_volcanoplot <- function(id,
                              datasets,
                              mae_name,
                              exclude_assays) {
  moduleServer(id, function(input, output, session) {
    experiment_data <- experimentSpecServer(
      "experiment",
      datasets = datasets,
      mae_name = mae_name
    )
    assay <- assaySpecServer(
      "assay",
      assays = experiment_data$assays,
      exclude_assays = exclude_assays
    )
    compare_group <- sampleVarSpecServer(
      "compare_group",
      experiment_name = experiment_data$name,
      original_data = experiment_data$data,
      num_levels = 2L,
      label_modal_title = "Please click to group into exactly 2 levels, first level is reference"
    )

    # When the filtered data set or the chosen experiment changes, update
    # the differential expression results.
    diff_expr <- reactive({
      object <- compare_group$experiment_data()
      compare_group <- compare_group$sample_var()
      method <- input$method

      req(
        object,
        method
      )
      validate(need(
        !is.null(compare_group),
        "Please select a group variable"
      ))

      hermes::diff_expression(
        object,
        group = compare_group,
        method = method
      )
    })

    output$plot <- renderPlot({
      diff_expr_result <- diff_expr()
      log2_fc_thresh <- input$log2_fc_thresh
      adj_p_val_thresh <- input$adj_p_val_thresh

      req(
        log2_fc_thresh,
        adj_p_val_thresh
      )

      hermes::autoplot(
        diff_expr_result,
        adj_p_val_thresh = adj_p_val_thresh,
        log2_fc_thresh = log2_fc_thresh
      )
    })

    # Display top genes if switched on.
    show_top_gene_diffexpr <- reactive({
      if (input$show_top_gene) {
        result <- diff_expr()
        with(
          result,
          data.frame(
            log2_fc = round(log2_fc, 2),
            stat = round(stat, 2),
            p_val = format.pval(p_val),
            adj_p_val = format.pval(adj_p_val),
            row.names = rownames(result)
          )
        )
      } else {
        NULL
      }
    })

    output$table <- DT::renderDT({
      DT::datatable(
        show_top_gene_diffexpr(),
        rownames = TRUE,
        options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
        caption = "Top Differentiated Genes"
      )
    })
  })
}

#' @describeIn tm_g_volcanoplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_volcanoplot()
#' }
sample_tm_g_volcanoplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- teal.data::dataset("MAE", mae)
  data <- teal.data::teal_data(mae_data)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_volcanoplot(
        label = "volcanoplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
