#' Module Input for Sample Variable Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the sample variable specification.
#'
#' @inheritParams module_arguments
#' @param label_vars (`string`)\cr label for the sample variable selection.
#' @param label_levels_button (`string`)\cr label for the levels combination button.
#'
#' @return The UI part.
#' @seealso [sampleVarSpecServer()] for the module server and a complete example.
#' @export
#'
#' @examples
#' sampleVarSpecInput("my_vars", label_vars = "Select faceting variable")
sampleVarSpecInput <- function(inputId,
                               label_vars = "Select sample variable",
                               label_levels_button = "Combine factor levels") {
  assert_string(inputId)
  assert_string(label_vars)
  assert_string(label_levels_button)

  ns <- NS(inputId)
  tagList(
    div(
      class = "row",
      div(
        class = "col-sm-8",
        tags$label(
          class = "control-label",
          label_vars
        )
      ),
      div(
        class = "col-sm-4",
        actionButton(
          ns("levels_button"),
          span(icon("fas fa-table")),
          title = label_levels_button,
          class = "pull-right list-genes"
        ),
        include_css_files(pattern = "*")
      )
    ),
    div(
      class = "custom-select-input",
      optionalSelectInput(
        ns("sample_var"),
        label = NULL,
        choices = "",
        multiple = FALSE
      )
    )
  )
}

#' Helper Function For Group List Creation
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function takes an assignment list and converts it to a
#' group list.
#'
#' @param x (named `list` of `character`)\cr input assignment list.
#' @return A combination list.
#'
#' @export
#'
#' @examples
#' assign_list <- list(
#'   "ASIAN" = "1",
#'   "BLACK OR AFRICAN AMERICAN" = "1",
#'   "MULTIPLE" = "2",
#'   "UNKNOWN" = "2",
#'   "WHITE" = "4"
#' )
#' objective_list <- list(
#'   "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
#'   "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
#'   "WHITE" = "WHITE"
#' )
#' result_list <- h_assign_to_group_list(assign_list)
#' stopifnot(identical(result_list, objective_list))
h_assign_to_group_list <- function(x) {
  assert_list(
    x,
    types = "character",
    any.missing = FALSE,
    names = "unique",
    unique = FALSE
  )
  x_vec <- unlist(x)
  x_split <- split(names(x_vec), x_vec)
  new_levels <- sapply(x_split, hermes::h_short_list, sep = "/")
  stats::setNames(x_split, new_levels)
}

#' Helper Function for Collapsing of Factor Levels
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Given a group list and a factor, this helper function collapses the
#' levels in the factor accordingly and also ensures that the resulting
#' levels are in the order given in the group list.
#'
#' @param x (`factor`)\cr original factor.
#' @param group_list (named `list` of `character`)\cr includes the collapsing
#'   specification.
#'
#' @return The transformed factor `x` with new levels.
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- factor(sample(
#'   c("ASIAN", "BLACK OR AFRICAN AMERICAN", "MULTIPLE", "UNKNOWN", "WHITE"),
#'   size = 30L,
#'   replace = TRUE
#' ))
#' group_list <- list(
#'   "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
#'   "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
#'   "WHITE" = "WHITE"
#' )
#' x_collapsed <- h_collapse_levels(x, group_list)
#' stopifnot(identical(levels(x_collapsed), names(group_list)))
h_collapse_levels <- function(x, group_list) {
  assert_factor(x)
  assert_list(group_list, names = "unique", types = "character")
  x_collapsed <- do.call(
    forcats::fct_collapse,
    args = c(
      list(.f = x),
      group_list
    )
  )
  factor(x_collapsed, levels = names(group_list))
}

#' Validation of Number of Levels
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This validation function checks that a given vector `x` is a factor with
#' the specified number of levels.
#'
#' @param x (`factor`)\cr factor to validate.
#' @param name (`string`)\cr name of `x` in the app.
#' @param n_levels (`count`)\cr required number of factor levels in `x`.
#'
#' @export
validate_n_levels <- function(x, name, n_levels) {
  validate(need(
    is.factor(x),
    paste("Variable", name, "is not a factor but a", class(x))
  ))
  assert_string(name, min.chars = 1L)
  assert_count(n_levels, positive = TRUE)
  validate(need(
    identical(n_levels, nlevels(x)),
    paste(
      "Please combine the original levels of", name,
      "into exactly", n_levels, "levels"
    )
  ))
}

#' Module Server for Sample Variable Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the sample variable specification.
#'
#' @inheritParams module_arguments
#' @param experiment_name (reactive `string`)\cr name of the input experiment.
#' @param original_data (reactive `SummarizedExperiment`)\cr input experiment where the
#'   sample variables extracted via [SummarizedExperiment::colData()] should be eligible for
#'   selection.
#' @param transformed_data (reactive `SummarizedExperiment`)\cr used when multiple sample
#'   variables can be selected in the app. In that case, pass here the pre-transformed data.
#' @param assign_lists (`reactivevalues`)\cr object to share factor level groupings across multiple
#'   sample variables.
#' @param num_levels (`count` or `NULL`)\cr required number of levels after combining original levels.
#'   If `NULL` then all numbers of levels are allowed.
#' @param label_modal_title (`string`)\cr title for the dialog that asks for the text input.
#'
#' @return Reactive [`SummarizedExperiment::SummarizedExperiment`] which can be used as
#'   input for the relevant `hermes` functions.
#' @seealso [sampleVarSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' ui <- function(id,
#'                datasets) {
#'   ns <- NS(id)
#'   mae <- datasets$get_data("MAE", filtered = FALSE)
#'   experiment_name_choices <- names(mae)
#'   teal.devel::standard_layout(
#'     encoding = div(
#'       selectInput(ns("experiment_name"), "Select experiment", experiment_name_choices),
#'       sampleVarSpecInput(ns("facet_var"), "Select faceting variable")
#'     ),
#'     output = plotOutput(ns("plot"))
#'   )
#' }
#' server <- function(input,
#'                    output,
#'                    session,
#'                    datasets) {
#'   experiment_data <- reactive({
#'     req(input$experiment_name)
#'     mae <- datasets$get_data("MAE", filtered = TRUE)
#'     object <- mae[[input$experiment_name]]
#'     SummarizedExperiment::colData(object) <- hermes::df_cols_to_factor(SummarizedExperiment::colData(object))
#'     object
#'   })
#'   facet_var_spec <- sampleVarSpecServer(
#'     "facet_var",
#'     experiment_name = reactive({
#'       input$experiment_name
#'     }),
#'     original_data = experiment_data
#'   )
#'   output$plot <- renderPlot({
#'     experiment_data_final <- facet_var_spec$experiment_data()
#'     facet_var <- facet_var_spec$sample_var()
#'     hermes::draw_boxplot(
#'       experiment_data_final,
#'       assay_name = "counts",
#'       genes = hermes::genes(experiment_data_final)[1],
#'       facet_var = facet_var
#'     )
#'   })
#' }
#' my_app <- function() {
#'   mae <- hermes::multi_assay_experiment
#'   mae_data <- dataset("MAE", mae)
#'   data <- teal_data(mae_data)
#'   app <- init(
#'     data = data,
#'     modules = root_modules(
#'       module(
#'         label = "sampleVarSpec example",
#'         server = server,
#'         ui = ui,
#'         filters = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#' if (interactive()) {
#'   my_app()
#' }
sampleVarSpecServer <- function(inputId,
                                experiment_name,
                                original_data,
                                transformed_data = original_data,
                                assign_lists = reactiveValues(),
                                num_levels = NULL,
                                label_modal_title = "Please click to group the original factor levels") {
  assert_string(inputId)
  assert_reactive(experiment_name)
  assert_reactive(original_data)
  assert_reactive(transformed_data)
  assert_class(assign_lists, "reactivevalues")
  assert_count(num_levels, null.ok = TRUE, positive = TRUE)
  assert_string(label_modal_title)

  moduleServer(inputId, function(input, output, session) {

    # The colData variables to choose the sample variable from.
    # if num_levels is specified then only take factors.
    col_data_vars <- eventReactive(experiment_name(), {
      object <- original_data()
      col_data <- SummarizedExperiment::colData(object)
      if (is.null(num_levels)) {
        names(col_data)
      } else {
        can_be_used <- sapply(col_data, is.factor)
        names(col_data)[can_be_used]
      }
    })

    # When the colData variables change, update the choices for sample_var.
    observeEvent(col_data_vars(), {
      col_data_vars <- col_data_vars()
      updateOptionalSelectInput(
        session,
        "sample_var",
        choices = col_data_vars,
        selected = character()
      )
    })

    # `reactiveValuees` object for storing experiment and colData variable
    # specific assignment lists.
    # Note that this should have experiments at the first level and then colData in the
    # second level.
    # assign_lists <- reactiveValues()

    # Reactive for the current combination. Takes the assignment list if available
    # and converts to combination list.
    current_combination <- reactive({
      experiment_name <- experiment_name()
      sample_var <- input$sample_var
      req(experiment_name)

      if (!is.null(sample_var)) {
        assign_list <- assign_lists[[experiment_name]][[sample_var]]
        if (!is.null(assign_list)) {
          h_assign_to_group_list(assign_list)
        } else {
          NULL
        }
      }
    })

    # Here we produce the final object by checking
    # if we should combine for this sample var.
    final_data <- reactive({
      sample_var <- input$sample_var
      original_data <- original_data()
      transformed_data <- transformed_data()
      current_combination <- current_combination()

      if (!is.null(sample_var)) {
        sample_var_vector <- SummarizedExperiment::colData(original_data)[[sample_var]]
        if (!is.null(current_combination)) {
          sample_var_vector <- h_collapse_levels(
            sample_var_vector,
            current_combination
          )
        }
        if (!is.null(num_levels)) {
          validate_n_levels(sample_var_vector, sample_var, num_levels)
        }
        SummarizedExperiment::colData(transformed_data)[[sample_var]] <- sample_var_vector
      }

      transformed_data
    })

    # Function to return the UI for a modal dialog with matrix input for combination
    # assignment.
    combModal <- function(sample_var_levels,
                          n_max_groups,
                          selected_groups) {
      if (is.null(selected_groups)) {
        selected_groups <- pmin(
          seq_along(sample_var_levels),
          n_max_groups
        )
      }
      modalDialog(
        shinyRadioMatrix::radioMatrixInput(
          session$ns("comb_assignment"),
          rowIDs = sample_var_levels,
          rowIDsName = "Original levels",
          rowLLabels = rep("", length = length(sample_var_levels)),
          choices = seq_len(n_max_groups),
          selected = selected_groups
        ),
        span(label_modal_title),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("ok"), "OK")
        ),
        include_js_files("checkbox.js")
      )
    }

    # Show modal when button is clicked and the current variable is a factor variable.
    observeEvent(input$levels_button, {
      sample_var <- input$sample_var
      original_data <- original_data()
      experiment_name <- experiment_name()

      req(experiment_name)

      if (!is.null(sample_var)) {
        current_sample_var <- SummarizedExperiment::colData(original_data)[[sample_var]]

        if (is.factor(current_sample_var)) {
          sample_var_levels <- levels(current_sample_var)

          # Note: here we make sure we load with previous choice so the user
          # does not constantly need to start from scratch again.
          selected_groups <- assign_lists[[experiment_name]][[sample_var]]

          showModal(combModal(
            sample_var_levels = sample_var_levels,
            n_max_groups = `if`(!is.null(num_levels), num_levels, length(sample_var_levels)),
            selected_groups = selected_groups
          ))
        } else {
          showNotification("Can only group levels for factor variables", type = "message")
        }
      }
    })

    # When OK button is pressed, save the settings, and remove the modal.
    observeEvent(input$ok, {
      experiment_name <- experiment_name()
      sample_var <- input$sample_var
      comb_assignment <- input$comb_assignment

      req(experiment_name, sample_var, comb_assignment)

      if (!is.null(num_levels) && !identical(length(unique(unlist(comb_assignment))), num_levels)) {
        showNotification(
          paste("Please group the original levels into exactly", num_levels, "levels"),
          type = "error"
        )
      } else {
        assign_lists[[experiment_name]][[sample_var]] <- comb_assignment
        removeModal()
      }
    })

    # Return both the reactives with the experiment data as well as the sample variable.
    list(
      experiment_data = final_data,
      sample_var = reactive({
        input$sample_var
      })
    )
  })
}

#' Module Server for Specification of Multiple Sample Variables
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' When multiple sample variables are used in a given module, then this
#' wrapper makes it much easier to specify in the server function.
#'
#' @param inputIds (`character`)\cr multiple input IDs corresponding to the
#'   different sample variables specified in the UI function.
#' @inheritParams sampleVarSpecServer
#' @param ... additional arguments as documented in [sampleVarSpecServer()],
#'   namely the mandatory `experiment_name` and the optional `num_levels` and
#'   `label_modal_title`. `transformed_data` and `assign_lists` should not be
#'   specified as they are already specified internally here.
#'
#' @return List with the final transformed `experiment_data` reactive and a
#'   list `vars` which contains the selected sample variables as reactives
#'   under their input ID.
#'
#' @export
#' @examples
#' \dontrun{
#' # In the server use:
#' sample_var_specs <- multiSampleVarSpecServer(
#'   inputIds = c("facet_var", "color_var"),
#'   experiment_name = reactive({
#'     input$experiment_name
#'   }),
#'   original_data = experiment_data
#' )
#' # Then can extract the transformed data and selected variables later:
#' experiment_data <- sample_var_specs$experiment_data()
#' facet_var <- sample_var_specs$vars$facet_var()
#' color_var <- sample_var_specs$vars$color_var()
#' }
multiSampleVarSpecServer <- function(inputIds,
                                     original_data,
                                     ...) {
  assert_character(inputIds, any.missing = FALSE, unique = TRUE)
  assign_lists <- reactiveValues()
  spec_list <- list()
  transformed_data <- original_data
  for (id in inputIds) {
    spec_list[[id]] <- sampleVarSpecServer(
      inputId = id,
      original_data = original_data,
      transformed_data = transformed_data,
      assign_lists = assign_lists,
      ...
    )
    transformed_data <- spec_list[[id]]$experiment_data
  }
  list(
    experiment_data = transformed_data,
    vars = lapply(spec_list, "[[", "sample_var")
  )
}
