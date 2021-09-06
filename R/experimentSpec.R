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
                                label_experiments = "Select experiment") {
  assert_string(inputId)
  assert_r6(datasets, "Datasets")
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

#' Module Server for Experiment Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the experiment specification.
#'
#' @inheritParams module_arguments
#' @param sample_vars_as_factors (`flag`)\cr whether to convert the sample variables
#'   (columns in `colData()` of the experiment) from character to factor variables.
#' @param with_mae_col_data (`flag`)\cr whether to include the `colData()` of the
#'   MAE into the experiment `colData()`.
#' @return List with the following reactive objects:
#'   - `data`: the [`hermes::AnyHermesData`] experiment.
#'   - `name`: the name of the experiment as selected by the user.
#'   - `genes`: the names of the genes in `data`.
#'   - `assays`: the names of the assays in `data`.
#'
#' @seealso [experimentSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' todo
experimentSpecServer <- function(inputId,
                                 datasets,
                                 mae_name,
                                 sample_vars_as_factors = TRUE,
                                 with_mae_col_data = TRUE) {
  assert_string(inputId)
  assert_r6(datasets, "Datasets")
  assert_string(mae_name, min.chars = 1L)
  assert_flag(sample_vars_as_factors)
  assert_flag(with_mae_col_data)

  moduleServer(inputId, function(input, output, session) {

    # When the filtered data set of the chosen experiment changes, update the
    # experiment data object.
    data <- reactive({
      name <- input$name
      req(name)

      mae <- datasets$get_data(mae_name, filtered = TRUE)
      object <- if (with_mae_col_data) {
        MultiAssayExperiment::getWithColData(mae, name)
      } else {
        mae[[name]]
      }
      validate(need(
        hermes::is_hermes_data(result),
        "Please first convert your experiment to HermesData class"
      ))
      if (sample_vars_as_factors) {
        SummarizedExperiment::colData(object) <-
          hermes::df_char_to_factor(SummarizedExperiment::colData(object))
      }
      object
    })

    # When the filtered data set or the chosen experiment changes, update
    # the calls that subset the genes of the chosen experiment data object.
    subset_calls <- reactive({
      name <- input$name
      req(name)

      filtered_mae <- datasets$get_filtered_datasets(mae_name)
      filter_states <- filtered_mae$get_filter_states(name)
      subset_queue <- filter_states$queue_get("subset")
      sapply(subset_queue, function(x) x$get_call())
    })

    # Only when the chosen gene subset changes, we recompute gene names.
    genes <- eventReactive(subset_calls(), ignoreNULL = FALSE, {
      data <- data()
      hermes::genes(data)
    })

    # When the chosen experiment changes, recompute the assay names.
    assays <- eventReactive(input$name, ignoreNULL = TRUE, {
      data <- data()
      SummarizedExperiment::assayNames(data)
    })

    return(
      list(
        data = data,
        name = reactive({input$name}), # nolint
        genes = genes,
        assays = assays
      )
    )
  })
}
