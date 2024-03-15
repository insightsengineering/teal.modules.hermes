#' Teal Module for PCA Analysis
#'
#' @description `r lifecycle::badge("experimental")`
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
#' data <- teal_data(MAE = hermes::multi_assay_experiment)
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_pca(
#'       label = "PCA plot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_pca <- function(label,
                     mae_name,
                     exclude_assays = character(),
                     pre_output = NULL,
                     post_output = NULL) {
  message("Initializing tm_g_pca")
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  teal::module(
    label = label,
    server = srv_g_pca,
    server_args = list(
      mae_name = mae_name,
      exclude_assays = exclude_assays
    ),
    ui = ui_g_pca,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    datanames = mae_name
  )
}

#' @describeIn tm_g_pca sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_pca <- function(id,
                     mae_name,
                     pre_output,
                     post_output) {
  ns <- NS(id)

  tagList(
    teal.widgets::standard_layout(
      include_css_files(pattern = "*"),
      encoding = tags$div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis of MAE:", tags$code(mae_name)),
        uiOutput(ns("experiment_ui")),
        assaySpecInput(ns("assay")),
        conditionalPanel(
          condition = "input.tab_selected == 'PCA'",
          ns = ns,
          sampleVarSpecInput(ns("color"), "Optional color variable"),
          selectizeInput(ns("x_var"), "Select X-axis PC", choices = ""),
          selectizeInput(ns("y_var"), "Select Y-axis PC", choices = "")
        ),
        teal.widgets::panel_group(
          teal.widgets::panel_item(
            input_id = "settings_item",
            collapsed = TRUE,
            title = "Additional Settings",
            tags$label("Use only Top Variance Genes"),
            shinyWidgets::switchInput(ns("filter_top"), value = FALSE, size = "mini"),
            conditionalPanel(
              condition = "input.filter_top",
              ns = ns,
              sliderInput(ns("n_top"), label = "Number of Top Genes", min = 10, max = 5000, value = 500)
            ),
            conditionalPanel(
              condition = "input.tab_selected == 'PCA'",
              ns = ns,
              tags$label("Show Variance %"),
              shinyWidgets::switchInput(ns("var_pct"), value = TRUE, size = "mini"),
              tags$label("Show Label"),
              shinyWidgets::switchInput(ns("label"), value = TRUE, size = "mini")
            ),
            conditionalPanel(
              condition = "input.tab_selected == 'PC and Sample Correlation'",
              ns = ns,
              tags$label("Cluster columns"),
              shinyWidgets::switchInput(ns("cluster_columns"), value = FALSE, size = "mini")
            ),
            tags$label("View Matrix"),
            shinyWidgets::switchInput(ns("show_matrix"), value = TRUE, size = "mini")
          )
        )
      ),
      output = tags$div(
        style = "display:flow-root",
        tabsetPanel(
          id = ns("tab_selected"),
          type = "tabs",
          tabPanel(
            "PCA",
            column(
              width = 12,
              tags$div(
                class = "my-5",
                teal.widgets::plot_with_settings_ui(ns("plot_pca"))
              ),
              DT::DTOutput(ns("table_pca"))
            )
          ),
          tabPanel(
            "PC and Sample Correlation",
            column(
              width = 12,
              tags$div(
                class = "my-5",
                teal.widgets::plot_with_settings_ui(ns("plot_cor"))
              ),
              DT::DTOutput(ns("table_cor"))
            )
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

#' @describeIn tm_g_pca sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_pca <- function(id,
                      data,
                      filter_panel_api,
                      reporter,
                      mae_name,
                      exclude_assays) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  assert_class(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(shiny::isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
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
    color <- sampleVarSpecServer(
      "color",
      experiment_name = experiment$name,
      original_data = experiment$data
    )

    # Total number of genes at the moment.
    n_genes <- reactive({
      experiment_data <- color$experiment_data()
      nrow(experiment_data)
    })

    # When the total number changes or gene filter is activated, update slider max.
    observeEvent(list(n_genes(), input$filter_top), {
      n_genes <- n_genes()
      filter_top <- input$filter_top
      if (filter_top) {
        n_top <- input$n_top
        updateSliderInput(
          session = session,
          inputId = "n_top",
          value = min(n_top, n_genes),
          max = n_genes
        )
      }
    })

    # When the chosen experiment or assay name changes, recompute the PC.
    pca_result <- reactive({
      experiment_data <- color$experiment_data()
      filter_top <- input$filter_top
      n_top <- input$n_top
      assay_name <- assay()

      validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))
      req(isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)))
      validate(need(
        ncol(experiment_data) > 2,
        "Sample size is too small. PCA needs more than 2 samples."
      ))
      validate(need(
        nrow(experiment_data) > 1,
        "Number of genes is too small. PCA needs more than 1 gene."
      ))

      hermes::calc_pca(experiment_data, assay_name, n_top = if (filter_top) n_top else NULL)
    })

    # When experiment or assay name changes, update choices for PCs in x_var and y_var.
    observeEvent(pca_result(), {
      pca_result_x <- pca_result()$x
      pc_choices <- seq_len(ncol(pca_result_x))

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

    # Compute correlation of PC with sample variables.
    cor_result <- reactive({
      pca_result <- pca_result()
      experiment_data <- color$experiment_data()

      hermes::correlate(pca_result, experiment_data)
    })

    # Compute & display PCA matrix table if show_matrix is TRUE.
    show_matrix_pca <- reactive({
      if (input$show_matrix) {
        pca_result_x <- pca_result()$x
        pca_result_x <- round(pca_result_x, 3)
        as.data.frame(pca_result_x)
      } else {
        NULL
      }
    })

    output$table_pca <- DT::renderDT({
      show_matrix_pca <- show_matrix_pca()
      DT::datatable(
        show_matrix_pca,
        rownames = TRUE,
        options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
        caption = "PCA Matrix"
      )
    })

    # Compute & display correlation matrix if show_matrix is TRUE
    show_matrix_cor <- reactive({
      if (input$show_matrix) {
        cor_result <- cor_result()
        cor_result <- round(cor_result, 3)
        as.data.frame(cor_result)
      } else {
        NULL
      }
    })
    output$table_cor <- DT::renderDT({
      show_matrix_cor <- show_matrix_cor()
      DT::datatable(
        show_matrix_cor,
        rownames = TRUE,
        options = list(scrollX = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100)),
        caption = "PC and Sample Correlation Matrix"
      )
    })

    # Render plot PCA output.
    plot_pca <- reactive({
      # Resolve all reactivity.
      pca_result <- pca_result()
      experiment_data <- color$experiment_data()
      x_var <- as.numeric(input$x_var)
      y_var <- as.numeric(input$y_var)
      data <- as.data.frame(SummarizedExperiment::colData(color$experiment_data()))
      color_var <- color$sample_var()
      assay_name <- assay()
      var_pct <- input$var_pct
      label <- input$label

      # Require which states need to be truthy.
      req(
        assay_name,
        # Note: The following statements are important to make sure the UI inputs have been updated.
        isTRUE(assay_name %in% SummarizedExperiment::assayNames(experiment_data)),
        is.null(color_var) || isTRUE(color_var %in% names(SummarizedExperiment::colData(experiment_data))),
        cancelOutput = FALSE
      )

      # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
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
        label.repel = label,
        label.show.legend = FALSE
      )
    })
    output$plot_pca <- renderPlot(plot_pca())

    pws_pca <- teal.widgets::plot_with_settings_srv(
      id = "plot_pca",
      plot_r = plot_pca
    )

    # render correlation heatmap
    plot_cor <- reactive({
      # Resolve all reactivity.
      cor_result <- cor_result()
      cluster_columns <- input$cluster_columns

      validate(need(
        !any(is.na(cor_result)),
        "Obtained NA results in the correlation matrix, therefore no plot can be produced"
      ))
      hermes::autoplot(
        object = cor_result,
        cluster_columns = cluster_columns
      )
    })

    pws_cor <- teal.widgets::plot_with_settings_srv(
      id = "plot_cor",
      plot_r = plot_cor
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- report_card_template(
          title = "PCA",
          label = label,
          with_filter = TRUE,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Selected Options", "header3")
        if (input$tab_selected == "PCA") {
          encodings_list <- list(
            "Experiment:",
            input$`experiment-name`,
            "\nAssay:",
            input$`assay-name`,
            "\nOptional Color Variable:",
            input$`color-sample_var`,
            "\nX-axis PC:",
            input$x_var,
            "\nY-axis PC:",
            input$y_var,
            "\nUse Top Variance Genes:",
            input$filter_top,
            "\nNumber of Top Genes:",
            input$n_top,
            "\nShow Variance %:",
            input$var_pct,
            "\nShow Matrix:",
            input$show_matrix,
            "\nShow Label:",
            input$label
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
          card$append_plot(plot_pca(), dim = pws_pca$dim())
          card$append_text("Table", "header3")
          card$append_table(show_matrix_pca())
        } else {
          encodings_list <- list(
            "Experiment:",
            input$`experiment-name`,
            "\nAssay:",
            input$`assay-name`,
            "\nUse Top Variance Genes:",
            input$filter_top,
            "\nNumber of Top Genes:",
            input$top_n,
            "\nCluster Columns:",
            paste0(input$cluster_columns, collapse = ", "),
            "\nShow Matrix:",
            input$show_matrix
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
          card$append_plot(plot_cor())
          card$append_plot(plot_cor(), dim = pws_cor$dim())
          card$append_text("Table", "header3")
          card$append_table(show_matrix_cor())
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

#' @describeIn tm_g_pca sample module function.
#' @export
#' @examples
#'
#' # Alternatively you can run the sample module with this function call:
#' if (interactive()) {
#'   sample_tm_g_pca()
#' }
sample_tm_g_pca <- function() {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      tm_g_pca(
        label = "pca",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
