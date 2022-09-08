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
                             data,
                             mae_name,
                             pre_output,
                             post_output) {
  ns <- NS(id)
  mae <- data[[mae_name]]

  teal.widgets::standard_layout(
    output = div(
      plotOutput(ns("plot")),
      DT::DTOutput(ns("table"))
    ),
    pre_output = pre_output,
    post_output = post_output,
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), data, mae_name),
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
                              data,
                              filter_panel_api,
                              reporter,
                              mae_name,
                              exclude_assays) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  checkmate::assert_class(filter_panel_api, "FilterPanelAPI")

  moduleServer(id, function(input, output, session) {
    experiment_data <- experimentSpecServer(
      "experiment",
      data = data,
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

    plot_r <- reactive({
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
    output$plot <- renderPlot(plot_r())

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

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Scatter Plot")
        card$append_text("Scatter Plot", "header2")
        card$append_fs(filter_panel_api$get_filter_state())
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nCompare Groups:",
          input$`compare_group-sample_var`,
          "\nShow Top Differentiated Genes:",
          input$show_top_gene,
          "\nMethod:",
          input$method,
          "\nLog2fold Change Threshold:",
          input$log2_fc_thresh,
          "\nAdjusted P-value Threshold:",
          input$adj_p_val_thresh
        )
        null_encodings_indices <- which(sapply(encodings_list, function(x) is.null(x) || x == ""))
        final_encodings <- if (length(null_encodings_indices) > 0) {
          null_encodings_indices_1 <- c(null_encodings_indices, null_encodings_indices - 1)
          paste(encodings_list[-null_encodings_indices_1], collapse = " ")
        } else {
          paste(encodings_list, collapse = " ")
        }

        card$append_text(final_encodings, style = "verbatim")
        card$append_text("Plot", "header3")
        card$append_plot(plot_r())
        if (isTRUE(input$show_top_gene)) {
          card$append_text("Table", "header3")
          card$append_table(show_top_gene_diffexpr())
        }
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
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
