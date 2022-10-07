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
#'   modules = modules(
#'     tm_g_boxplot(
#'       label = "boxplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_boxplot <- function(label,
                         mae_name,
                         exclude_assays = character(),
                         summary_funs = list(
                           None = NULL,
                           Mean = colMeans,
                           Median = matrixStats::colMedians,
                           Max = matrixStats::colMaxs
                         ),
                         pre_output = NULL,
                         post_output = NULL) {
  logger::log_info("Initializing tm_g_boxplot")
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_summary_funs(summary_funs, null.ok = TRUE)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  teal::module(
    label = label,
    server = srv_g_boxplot,
    server_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_boxplot,
    ui_args = list(
      mae_name = mae_name,
      summary_funs = summary_funs,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = mae_name
  )
}

#' @describeIn tm_g_boxplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_boxplot <- function(id,
                         datasets,
                         mae_name,
                         summary_funs,
                         pre_output,
                         post_output) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), summary_funs),
      tags$label("Jitter"),
      shinyWidgets::switchInput(ns("jitter"), value = FALSE, size = "mini"),
      tags$label("Violin Plot"),
      shinyWidgets::switchInput(ns("violin"), value = FALSE, size = "mini"),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("strat"), "Optional stratifying variable"),
          sampleVarSpecInput(ns("color"), "Optional color variable"),
          sampleVarSpecInput(ns("facet"), "Optional facet variable")
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_boxplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_boxplot <- function(id,
                          datasets,
                          reporter,
                          mae_name,
                          exclude_assays,
                          summary_funs) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
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
      inputIds = c("strat", "color", "facet"),
      experiment_name = experiment$name,
      original_data = experiment$data
    )
    genes <- geneSpecServer(
      "genes",
      funs = summary_funs,
      gene_choices = experiment$genes
    )
    plot_r <- reactive({
      # Resolve all reactivity.
      experiment_data <- multi$experiment_data()
      strat <- multi$vars$strat()
      genes <- genes()
      facet <- multi$vars$facet()
      color <- multi$vars$color()
      assay <- assay()
      jitter <- input$jitter
      violin <- input$violin

      req(
        assay,
        # Note: The following statements are important to make sure the UI inputs have been updated.
        isTRUE(assay %in% SummarizedExperiment::assayNames(experiment_data)),
        is.null(facet) || isTRUE(facet %in% names(SummarizedExperiment::colData(experiment_data))),
        is.null(color) || isTRUE(color %in% names(SummarizedExperiment::colData(experiment_data))),
        is.null(strat) || isTRUE(strat %in% names(SummarizedExperiment::colData(experiment_data))),
        cancelOutput = FALSE
      )

      validate_gene_spec(genes, rownames(experiment_data))

      hermes::draw_boxplot(
        object = experiment_data,
        assay_name = assay,
        genes = genes,
        x_var = strat,
        facet_var = facet,
        color_var = color,
        jitter = jitter,
        violin = violin
      )
    })
    output$plot <- renderPlot(plot_r())

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Boxplot")
        card$append_text("Boxplot", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Selected Options", "header3")
        encodings_list <- list(
          "Experiment:",
          input$`experiment-name`,
          "\nAssay:",
          input$`assay-name`,
          "\nFacetting Variable:",
          input$`facet-sample_var`,
          "\nGenes Selected:",
          paste0(genes()$get_gene_labels(), collapse = ", "),
          "\nGene Summary:",
          input$`genes-fun_name`,
          "\nJitter:",
          input$jitter,
          "\nViolin:",
          input$violin,
          "\nOptional Stratifying Variable:",
          input$`strat-sample_var`,
          "\nOptional Color Variable:",
          input$`color-sample_var`,
          "\nOptional Facet Variable:",
          input$`facet-sample_var`
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

#' @describeIn tm_g_boxplot sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_boxplot()
#' }
sample_tm_g_boxplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- teal.data::dataset("MAE", mae)
  data <- teal.data::teal_data(mae_data)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_boxplot(
        label = "boxplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
