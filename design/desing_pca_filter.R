# module
tm_g_pca2 <- function(label,
                      mae_name,
                      pre_output = NULL,
                      post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
    label = label,
    server = srv_g_pca2,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_pca2,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    datanames = "all"
  )
}

# ui

ui_g_pca2 <- function(id,
                      datasets,
                      mae_name,
                      pre_output,
                      post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)

  tagList(
    teal.widgets::standard_layout(
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        helpText("Analysis of MAE:", tags$code(mae_name)),
        selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
        selectInput(ns("assay_name"), "Select assay", choices = ""),
        conditionalPanel(
          condition = "input.tab_selected == 'PCA'",
          ns = ns,
          teal.widgets::optionalSelectInput(ns("color_var"), "Optional color variable"),
          selectizeInput(ns("x_var"), "Select X-axis PC", choices = ""),
          selectizeInput(ns("y_var"), "Select Y-axis PC", choices = ""),
        ),
        bslib::input_switch(
          ns("filter_top"),
          label = "Use only Top Variance Genes",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.filter_top",
          ns = ns,
          sliderInput(ns("n_top"), label = "Number of Top Genes", min = 10, max = 5000, value = 500)
        ),
        bslib::input_switch(
          ns("var_pct"),
          label = "Show Variance %",
          value = TRUE
        ),
        bslib::input_switch(
          ns("label"),
          label = "Show Label",
          value = TRUE
        ),
        conditionalPanel(
          condition = "input.tab_selected == 'PC and Sample Correlation'",
          ns = ns,
          bslib::input_switch(
            ns("cluster_columns"),
            label = "Cluster columns",
            value = FALSE
          )
        ),
        bslib::input_switch(
          ns("show_matrix"),
          label = "View Matrix",
          value = TRUE
        )
      ),
      output = tagList(
        tabsetPanel(
          id = ns("tab_selected"),
          type = "tabs",
          tabPanel(
            "PCA",
            bslib::page_fluid(
              tags$div(
                class = "my-5",
                plotOutput(ns("plot_pca"))
              ),
              DT::DTOutput(ns("table_pca"))
            )
          ),
          tabPanel(
            "PC and Sample Correlation",
            bslib::page_fluid(
              tags$div(
                class = "my-5",
                plotOutput(ns("plot_cor"))
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

# server

srv_g_pca2 <- function(input,
                       output,
                       session,
                       datasets,
                       mae_name) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name) # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the call that creates the chosen experiment data object.
  experiment_call <- reactive({
    req(input$experiment_name) # Important to avoid running into NULL here.

    dat <- datasets$get_filtered_dataset(mae_name)
    dat$get_filter_states(input$experiment_name)$get_call()
  })

  # Total number of genes at the moment.
  n_genes <- reactive({
    experiment_data <- experiment_data()
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

    teal.widgets::updateOptionalSelectInput(
      session,
      "color_var",
      choices = color_var_choices,
      selected = character()
    )
  })

  # When the chosen experiment or assay name changes, recompute the PC.
  pca_result <- reactive({
    experiment_data <- experiment_data()
    assay_name <- input$assay_name
    filter_top <- input$filter_top
    n_top <- input$n_top

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
    experiment_data <- experiment_data()

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
      cor_result <- round(cor_result(), 3)
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
    experiment_data <- experiment_data()
    x_var <- as.numeric(input$x_var)
    y_var <- as.numeric(input$y_var)
    data <- as.data.frame(SummarizedExperiment::colData(experiment_data()))
    color_var <- input$color_var
    assay_name <- input$assay_name
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

sample_tm_g_pca2 <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = modules(
      static = {
        tm_g_pca2(
          label = "pca",
          mae_name = "MAE"
        )
      }
    )
  )
  shinyApp(app$ui, app$server)
}

sample_tm_g_pca2()
