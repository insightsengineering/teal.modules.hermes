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
#' mae <- hermes::multi_assay_experiment
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'    tm_g_pca(
#'         label = "PCA plot",
#'         mae_name = "MAE"
#'    )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_pca <- function(label,
                     mae_name,
                     exclude_assays = character(),
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
      mae_name = mae_name,
      exclude_assays = exclude_assays
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
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      conditionalPanel(
        condition = "input.tab_selected == 'PCA'",
        ns = ns,
        sampleVarSpecInput(ns("color"), "Optional color variable"),
        selectizeInput(ns("x_var"), "Select X-axis PC", choices = ""),
        selectizeInput(ns("y_var"), "Select Y-axis PC", choices = "")
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
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
    output = tagList(
      tabsetPanel(
        id = ns("tab_selected"),
        type = "tabs",
        tabPanel(
          "PCA",
          column(
            width = 12,
            div(style = "height:20px;"),
            plotOutput(ns("plot_pca")),
            br(), br(), br(),
            DT::DTOutput(ns("table_pca"))
          )
        ),
        tabPanel(
          "PC and Sample Correlation",
          column(
            width = 12,
            div(style = "height:20px;"),
            plotOutput(ns("plot_cor")),
            br(), br(), br(),
            DT::DTOutput(ns("table_cor"))
          )
        )
      )
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
                      mae_name,
                      exclude_assays) {
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
      ncol(SummarizedExperiment::assay(experiment_data)) > 2,
      "Sample size is too small. PCA needs more than 2 samples."
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
  output$plot_pca <- renderPlot({
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
      label.repel = label
    )
  })

  # render correlation heatmap
  output$plot_cor <- renderPlot({
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
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      static = {
        tm_g_pca(
          label = "pca",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}
