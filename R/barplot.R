#' Teal Module for RNA-seq Barplot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This module provides an interactive barplot for RNA-seq gene expression
#' analysis.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::module
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' data <- teal_data(MAE = hermes::multi_assay_experiment)
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
                         post_output = NULL,
                         .test = FALSE,
                         transformators = list()) {
  message("Initializing tm_g_barplot")
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)
  assert_flag(.test)

  module(
    label = label,
    server = srv_g_barplot,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays,
      summary_funs = summary_funs,
      .test = .test
    ),
    ui = ui_g_barplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output,
      .test = .test
    ),
    transformators = transformators,
    datanames = mae_name
  )
}

#' @describeIn tm_g_barplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_barplot <- function(id,
                         mae_name,
                         summary_funs,
                         pre_output,
                         post_output,
                         .test = FALSE) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      uiOutput(ns("experiment_ui")),
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
    output = div(
      if (.test) verbatimTextOutput(ns("table")) else NULL,
      teal.widgets::plot_with_settings_ui(ns("plot"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_barplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_barplot <- function(id,
                          data,
                          filter_panel_api,
                          reporter,
                          mae_name,
                          exclude_assays,
                          summary_funs,
                          .test = FALSE) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")
  assert_flag(.test)
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.hermes")
    output$experiment_ui <- renderUI({
      experimentSpecInput(session$ns("experiment"), data, mae_name)
    })
    experiment <- experimentSpecServer(
      "experiment",
      data = data,
      filter_panel_api = filter_panel_api,
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

    plot_r <- reactive({
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

    output$plot <- renderPlot(plot_r())

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r
    )

    if (.test) {
      table_r <- reactive({
        utils::str(layer_data(plot_r()))
      })
      output$table <- renderPrint(table_r())
    }

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "Barplot",
          label = label,
          with_filter = TRUE,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nFacetting Variable:",
          input$`facet-sample_var`,
          "\nGenes Selected:",
          paste0(x()$get_gene_labels(), collapse = ", "),
          "\nGene Summary:",
          input$`x-fun_name`,
          "\nQuantiles:",
          paste0(input$percentiles, collapse = ", "),
          "\nOptional Fill Variable:",
          input$`fill-sample_var`
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
        card$append_plot(plot_r(), dim = pws$dim())
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

#' @describeIn tm_g_barplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_barplot()
#' }
sample_tm_g_barplot <- function(.test = FALSE) {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_barplot(
        label = "barplot",
        mae_name = "MAE",
        .test = .test
      )
    )
  )
  shinyApp(app$ui, app$server)
}
