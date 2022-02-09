#' Module Input for Experiment Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the experiment specification.
#'
#' @inheritParams module_arguments
#' @param label_experiments (`string`)\cr label for the experiment selection.
#'
#' @return The UI part.
#' @seealso [experimentSpecServer()] for the module server and a complete example.
#' @export
experimentSpecInput <- function(inputId,
                                datasets,
                                mae_name,
                                label_experiments = "Select Experiment") {
  assert_string(inputId)
  assert_r6(datasets)
  assert_string(mae_name, min.chars = 1L)
  assert_string(label_experiments, min.chars = 1L)

  mae <- datasets$get_data(mae_name, filtered = FALSE)
  name_choices <- names(mae)

  ns <- NS(inputId)
  selectInput(
    inputId = ns("name"),
    label = label_experiments,
    choices = name_choices
  )
}

#' Helper Function to Order Gene Choices
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The possible gene choices are ordered as follows. First come all genes which
#' have a non-empty name, ordered by their name alphabetically. Last come
#' all genes with an empty name, ordered by their ID alphabetically.
#'
#' @param genes (`data.frame`)\cr containing `id` and `name` columns of the
#'   gene choices. Note that no missing values are allowed.
#'
#' @return The ordered `data.frame`.
#' @export
#'
#' @examples
#' genes <- data.frame(
#'   id = c("7", "1", "2", "345346", "0"),
#'   name = c("e", "", "c", "", "a")
#' )
#' h_order_genes(genes)
h_order_genes <- function(genes) {
  assert_data_frame(genes, types = "character", any.missing = FALSE)
  assert_set_equal(names(genes), c("id", "name"))

  has_empty_name <- genes$name == ""
  first_genes <- which(!has_empty_name)[order(genes[!has_empty_name, ]$name)]
  last_genes <- which(has_empty_name)[order(genes[has_empty_name, ]$id)]
  genes[c(first_genes, last_genes), ]
}

#' Helper Function to Format Gene Choices
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Given a [`hermes::AnyHermesData`] data object, as well as the annotation
#' column name to use as gene name, this function formats the contained genes
#' as a `data.frame` ready for consumption in [h_order_genes()] e.g.
#'
#' @details
#' Note that missing names or names that only contain whitespace
#' are replaced by empty strings for consistency and better labeling in the
#' UI downstream
#'
#' @inheritParams function_arguments
#' @inheritParams experimentSpecServer
#'
#' @return A `data.frame` with `id` and `name` columns containing all genes from
#'   `object`.
#' @export
#'
#' @examples
#' object <- hermes::hermes_data[1:10, ]
#' h_gene_data(object, "symbol")
h_gene_data <- function(object, name_annotation) {
  assert_true(hermes::is_hermes_data(object))
  assert_string(name_annotation, null.ok = TRUE)

  gene_ids <- hermes::genes(object)
  gene_names <- if (!is.null(name_annotation)) {
    annotation_data <- hermes::annotation(object)
    assert_subset(name_annotation, names(annotation_data))
    annotation_vector <- annotation_data[[name_annotation]]
    annotation_missing <- is.na(annotation_vector) | grepl("^\\s+$", annotation_vector)
    annotation_vector[annotation_missing] <- ""
    annotation_vector
  } else {
    ""
  }
  data.frame(
    id = gene_ids,
    name = gene_names
  )
}

#' Module Server for Experiment Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the experiment specification.
#'
#' @inheritParams module_arguments
#' @param name_annotation (`string` or `NULL`)\cr which annotation column to use as name
#'   to return in the `genes` data. If `NULL`, then the `name` column will be set to empty
#'   strings.
#' @param sample_vars_as_factors (`flag`)\cr whether to convert the sample variables
#'   (columns in `colData()` of the experiment) from character to factor variables.
#' @param with_mae_col_data (`flag`)\cr whether to include the `colData()` of the
#'   MAE into the experiment `colData()`.
#' @return List with the following reactive objects:
#'   - `data`: the [`hermes::AnyHermesData`] experiment.
#'   - `name`: the name of the experiment as selected by the user.
#'   - `genes`: a `data.frame` with the genes in `data`, with columns `id` and `name`.
#'   - `assays`: the names of the assays in `data`.
#'
#' @seealso [experimentSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' ui <- function(id,
#'                datasets,
#'                mae_name) {
#'   ns <- NS(id)
#'   teal.devel::standard_layout(
#'     encoding = div(
#'       experimentSpecInput(
#'         ns("my_experiment"),
#'         datasets,
#'         mae_name,
#'         label_experiments = "Please choose experiment"
#'       ),
#'       selectInput(
#'         ns("property"),
#'         "Please choose property",
#'         c("data", "name", "genes", "assays")
#'       )
#'     ),
#'     output = div(
#'       verbatimTextOutput(ns("summary")),
#'       verbatimTextOutput(ns("head"))
#'     )
#'   )
#' }
#'
#' server <- function(id,
#'                    datasets,
#'                    mae_name) {
#'   moduleServer(id, function(input, output, session) {
#'     experiment <- experimentSpecServer(
#'       "my_experiment",
#'       datasets,
#'       mae_name
#'     )
#'     result <- reactive({
#'       switch(input$property,
#'         data = experiment$data(),
#'         name = experiment$name(),
#'         genes = experiment$genes(),
#'         assays = experiment$assays()
#'       )
#'     })
#'     output$summary <- renderPrint({
#'       result <- result()
#'       hermes::summary(result)
#'     })
#'     output$head <- renderPrint({
#'       result <- result()
#'       utils::head(result)
#'     })
#'   })
#' }
#'
#' my_app <- function() {
#'   mae <- hermes::multi_assay_experiment
#'   mae_name <- "MAE"
#'   mae_data <- dataset(mae_name, mae)
#'   data <- teal_data(mae_data)
#'   app <- init(
#'     data = data,
#'     modules = modules(
#'       module(
#'         label = "experimentSpec example",
#'         server = server,
#'         server_args = list(mae_name = mae_name),
#'         ui = ui,
#'         ui_args = list(mae_name = mae_name),
#'         filters = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#' if (interactive()) {
#'   my_app()
#' }
experimentSpecServer <- function(id,
                                 datasets,
                                 mae_name,
                                 name_annotation = "symbol",
                                 sample_vars_as_factors = TRUE,
                                 with_mae_col_data = TRUE) {
  assert_string(id)
  assert_r6(datasets)
  assert_string(mae_name, min.chars = 1L)
  assert_string(name_annotation, min.chars = 1L, null.ok = TRUE)
  assert_flag(sample_vars_as_factors)
  assert_flag(with_mae_col_data)

  moduleServer(id, function(input, output, session) {

    # When the filtered data set of the chosen experiment changes, update the
    # experiment data object.
    data <- reactive({
      name <- input$name
      req(name)

      mae <- datasets$get_data(mae_name, filtered = TRUE)
      orig_object <- mae[[name]]
      validate(need(
        hermes::is_hermes_data(orig_object),
        "Please first convert your experiment to HermesData class"
      ))
      validate(need(
        !hermes::isEmpty(orig_object),
        "No genes or samples included in this experiment, please adjust filters"
      ))
      object <- if (with_mae_col_data) {
        MultiAssayExperiment::getWithColData(mae, name)
      } else {
        orig_object
      }
      if (sample_vars_as_factors) {
        SummarizedExperiment::colData(object) <-
          hermes::df_cols_to_factor(SummarizedExperiment::colData(object))
      }
      object
    })

    # When the filtered data set or the chosen experiment changes, update
    # the calls that subset the genes of the chosen experiment data object.
    subset_calls <- reactive({
      name <- input$name
      req(name)

      filtered_mae <- datasets$get_filtered_dataset(mae_name)
      filter_states <- filtered_mae$get_filter_states(name)
      subset_queue <- filter_states$queue_get("subset")
      sapply(subset_queue, function(x) x$get_call())
    })

    # Only when the chosen gene subset changes, we recompute gene choices
    genes <- eventReactive(subset_calls(), ignoreNULL = FALSE, {
      data <- data()
      genes <- h_gene_data(data, name_annotation)
      h_order_genes(genes)
    })

    # When the chosen experiment changes, recompute the assay names.
    assays <- eventReactive(input$name, ignoreNULL = TRUE, {
      data <- data()
      SummarizedExperiment::assayNames(data)
    })


    list(
      data = data,
      name = reactive({
        input$name
      }), # nolint
      genes = genes,
      assays = assays
    )
  })
}
