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
#' mae <- hermes::multi_assay_experiment
#' mae_data <- dataset("MAE", mae)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'       label = "scatterplot",
#'       mae_name = "MAE"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
  assert_string(label)
  assert_string(mae_name)
  assert_character(exclude_assays, any.missing = FALSE)
  assert_list(
    summary_funs,
    types = "function",
    min.len = 1L,
    unique = TRUE,
    any.missing = FALSE,
    names = "unique"
  )
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)

  module(
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
    filters = "all"
  )
}

#' @describeIn tm_g_scatterplot sets up the user interface.
#' @inheritParams module_arguments
#' @export
ui_g_scatterplot <- function(id,
                             datasets,
                             mae_name,
                             summary_funs,
                             pre_output,
                             post_output) {
  ns <- NS(id)
  mae <- datasets$get_data(mae_name, filtered = FALSE)
  experiment_name_choices <- names(mae)

  smooth_method_choices <- c(
    Linear = "lm",
    Loess = "loess",
    None = "none"
  )

  teal.devel::standard_layout(
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis of MAE:", tags$code(mae_name)),
      selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
      selectInput(ns("assay_name"), "Select assay", choices = ""),
      geneSpecInput(ns("x_spec"), summary_funs, label_genes = "Select x gene(s)"),
      geneSpecInput(ns("y_spec"), summary_funs, label_genes = "Select y gene(s)"),
      teal.devel::panel_group(
        input_id = "settings_group",
        teal.devel::panel_item(
          input_id = "settings_item",
          collapsed = TRUE,
          title = "Additional Settings",
          sampleVarSpecInput(ns("color_var"), "Optional color variable"),
          sampleVarSpecInput(ns("facet_var"), "Optional facet variable"),
          selectInput(ns("smooth_method"), "Select smoother", smooth_method_choices)
        )
      )
    ),
    output = plotOutput(ns("plot")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @describeIn tm_g_scatterplot sets up the server with reactive graph.
#' @inheritParams module_arguments
#' @export
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              mae_name,
                              exclude_assays,
                              summary_funs) {
  # When the filtered data set of the chosen experiment changes, update the
  # experiment data object.
  experiment_data <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    mae <- datasets$get_data(mae_name, filtered = TRUE)
    mae[[input$experiment_name]]
  })

  # When the filtered data set or the chosen experiment changes, update
  # the calls that subset the genes of the chosen experiment data object.
  experiment_subset_calls <- reactive({
    req(input$experiment_name)  # Important to avoid running into NULL here.

    filtered_mae <- datasets$get_filtered_datasets(mae_name)
    filter_states <- filtered_mae$get_filter_states(input$experiment_name)
    subset_queue <- filter_states$queue_get("subset")
    sapply(subset_queue, function(x) x$get_call())
  })

  # When the chosen gene subset changes, we recompute gene names.
  genes <- eventReactive(experiment_subset_calls(), ignoreNULL = FALSE, {
    object <- experiment_data()
    gene_ids <- rownames(object)
    gene_ids
  })

  # When the chosen experiment changes, recompute the assay names.
  assay_names <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
    object <- experiment_data()
    SummarizedExperiment::assayNames(object)
  })

  # When the assay names change, update the choices for assay.
  observeEvent(assay_names(), {
    assay_name_choices <- setdiff(
      assay_names(),
      exclude_assays
    )

    updateSelectInput(
      session,
      "assay_name",
      choices = assay_name_choices
    )
  })

  facet_var_spec <- sampleVarSpecServer(
    "facet_var",
    experiment_name = reactive({input$experiment_name}),
    experiment_data = experiment_data
  )
  color_var_spec <- sampleVarSpecServer(
    "color_var",
    experiment_name = reactive({input$experiment_name}),
    experiment_data = facet_var_spec$experiment_data  # Note the starting point here.
  )
  x_spec <- geneSpecServer("x_spec", summary_funs, genes)
  y_spec <- geneSpecServer("y_spec", summary_funs, genes)

  output$plot <- renderPlot({
    # Resolve all reactivity.
    experiment_data <- color_var_spec$experiment_data()
    x_spec <- x_spec()
    y_spec <- y_spec()
    facet_var <- facet_var_spec$sample_var()
    color_var <- color_var_spec$sample_var()
    assay_name <- input$assay_name
    smooth_method <- input$smooth_method

    validate(need(
      !is_blank(assay_name),
      "no assays are available for this experiment, please choose another experiment"
    ))
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

    # Validate and give useful messages to the user. Note: no need to duplicate here req() from above.
    validate(need(hermes::is_hermes_data(experiment_data), "please use HermesData() on input experiments"))

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
}

#' @describeIn tm_g_scatterplot sample module function.
#' @export
#' @examples
#' \dontrun{
#' # Alternatively you can run the sample module with this function call:
#' sample_tm_g_scatterplot()
#' }
sample_tm_g_scatterplot <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      tm_g_scatterplot(
        label = "scatterplot",
        mae_name = "MAE"
      )
    )
  )
  shinyApp(app$ui, app$server)
}


