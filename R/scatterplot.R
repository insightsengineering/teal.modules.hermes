#' Teal Module for RNA-seq Scatterplot
#'
#' @description `r lifecycle::badge("experimental")`
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
#' data <- teal_data(MAE = hermes::multi_assay_experiment)
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_scatterplot(
#'       label = "scatterplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                             mae_name,
                             exclude_assays = "counts",
                             summary_funs = list(
                               Mean = colMeans,
                               Median = matrixStats::colMedians,
                               Max = matrixStats::colMaxs
                             ),
                             pre_output = NULL,
                             post_output = NULL) {
  message("Initializing tm_g_scatterplot")
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  teal::module(
    label = label,
    server = srv_g_scatterplot,
    server_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_scatterplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    datanames = mae_name
  )
}

#' @describeIn tm_g_scatterplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_scatterplot <- function(id,
                             mae_name,
                             summary_funs,
                             pre_output,
                             post_output) {
  ns <- NS(id)

  smooth_method_choices <- c(
    Linear = "lm",
    Loess = "loess",
    None = "none"
  )

  teal.widgets::standard_layout(
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      uiOutput(ns("experiment_ui")),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select x Gene(s)"),
      geneSpecInput(ns("y_spec"), summary_funs, label_genes = "Select y Gene(s)"),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("color_var"), "Optional color variable"),
          sampleVarSpecInput(ns("facet_var"), "Optional facet variable"),
          selectInput(ns("smooth_method"), "Select smoother", smooth_method_choices)
        )
      )
    ),
    output = teal.widgets::plot_with_settings_ui(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_scatterplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_scatterplot <- function(id,
                              data,
                              filter_panel_api,
                              reporter,
                              mae_name,
                              exclude_assays,
                              summary_funs) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$experiment_ui <- renderUI({
      experimentSpecInput(ns("experiment"), data, mae_name)
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
    sample_var_specs <- multiSampleVarSpecServer(
      inputIds = c("facet_var", "color_var"),
      experiment_name = experiment$name,
      original_data = experiment$data
    )
    x_spec <- geneSpecServer("x_spec", summary_funs, experiment$genes)
    y_spec <- geneSpecServer("y_spec", summary_funs, experiment$genes)

    plot_r <- reactive({
      # Resolve all reactivity.
      experiment_data <- sample_var_specs$experiment_data()
      x_spec <- x_spec()
      y_spec <- y_spec()
      facet_var <- sample_var_specs$vars$facet_var()
      color_var <- sample_var_specs$vars$color_var()
      assay_name <- assay()
      smooth_method <- input$smooth_method

      validate_gene_spec(x_spec, rownames(experiment_data))
      validate_gene_spec(y_spec, rownames(experiment_data))

      # Require which states need to be truthy.
      req(
        smooth_method,
        # Note: The following statements are important to make sure the UI inputs have been updated.
        isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
        is.null(facet_var) || isTRUE(facet_var %in% names(SummarizedExperiment::colData(experiment_data))),
        is.null(color_var) || isTRUE(color_var %in% names(SummarizedExperiment::colData(experiment_data))),
        cancelOutput = FALSE
      )

      hermes::draw_scatterplot(
        object = experiment_data,
        assay_name = assay_name,
        x_spec = x_spec,
        y_spec = y_spec,
        facet_var = facet_var,
        color_var = color_var,
        smooth_method = smooth_method
      )
    })
    output$plot <- renderPlot(plot_r())

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = plot_r
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "Scatter Plot",
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
          "\nX Genes Selected:",
          paste0(x_spec()$get_gene_labels(), collapse = ", "),
          "\nX Genes Summary:",
          input$`x_spec-fun_name`,
          "\nY Genes Selected:",
          paste0(y_spec()$get_gene_labels(), collapse = ", "),
          "\nY Genes Summary:",
          input$`y_spec-fun_name`,
          "\nOptional Color Variable:",
          input$`color_var-sample_var`,
          "\nOptional Facetting Variable:",
          input$`facet_var-sample_var`,
          "\nSmoother:",
          input$smooth_method
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

#' @describeIn tm_g_scatterplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_scatterplot()
#' }
sample_tm_g_scatterplot <- function() {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
