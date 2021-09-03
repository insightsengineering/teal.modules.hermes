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
          span(icon("font fa-object-ungroup")),
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

#' Module Server for Sample Variable Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the sample variable specification.
#'
#' @inheritParams module_arguments
#' @param object (reactive `SummarizedExperiment`)\cr input experiment where the
#'   sample variables extracted via [SummarizedExperiment::colData()] should be eligible for
#'   selection.
#' @param label_modal_title (`string`)\cr title for the dialog that asks for the text input.
#'
#' @return Reactive [`SummarizedExperiment::SummarizedExperiment`] which can be used as
#'   input for the relevant `hermes` functions.
#' @seealso [sampleVarSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' todo

sampleVarSpecServer <- function(inputId,
                                object,
                                label_modal_title = "Please click to group the original factor levels") {
  assert_string(inputId)
  assert_reactive(object)
  assert_string(label_modal_title)

  moduleServer(inputId, function(input, output, session) {
    # When the chosen experiment changes, recompute the colData variables.
    col_data_vars <- eventReactive(input$experiment_name, ignoreNULL = TRUE, {
      object <- experiment_data()
      names(SummarizedExperiment::colData(object))
    })
    # When the colData variables change, update the choices for facet_var.
    observeEvent(col_data_vars(), {
      col_data_vars <- col_data_vars()
      updateOptionalSelectInput(
        session,
        "facet_var",
        choices = col_data_vars,
        selected = character()
      )
    })

    # `reactiveValuees` object for storing experiment and colData variable
    # specific assignment lists.
    # Note that this should have experiments at the first level and then colData in the
    # second level.
    assign_lists <- reactiveValues()
    # Reactive for the current combination. Takes the assignment list if available
    # and converts to combination list.
    current_combination <- reactive({
      experiment_name <- input$experiment_name
      facet_var <- input$facet_var
      req(experiment_name, facet_var)

      assign_list <- assign_lists[[experiment_name]][[facet_var]]
      if (!is.null(assign_list)) {
        h_assign_to_group_list(assign_list)
      } else {
        NULL
      }
    })
    # Here we produce the final object by checking
    # if we should combine for this facet var.
    experiment_data_final <- reactive({
      facet_var <- input$facet_var
      experiment_data <- experiment_data()
      current_combination <- current_combination()

      req(facet_var)

      if (!is.null(current_combination)) {
        colData(experiment_data)[[facet_var]] <- do.call(
          forcats::fct_collapse,
          args = c(
            list(.f = colData(experiment_data)[[facet_var]]),
            current_combination
          )
        )
      }
      experiment_data
    })

    # Function to return the UI for a modal dialog with matrix input for combination
    # assignment.
    combModal <- function(facet_levels,
                          n_max_groups,
                          selected_groups) {
      if (is.null(selected_groups)) {
        selected_groups <- seq_len(n_max_groups)
      }
      modalDialog(
        shinyRadioMatrix::radioMatrixInput(
          session$ns("comb_assignment"),
          rowIDs = facet_levels,
          rowLLabels = rep("", length = length(facet_levels)),
          choices = seq_len(n_max_groups),
          selected = selected_groups
        ),
        span(
          "Please click to group the original factor levels"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("ok"), "OK")
        ),
        include_js_files("checkbox.js")
      )
    }

    # Show modal when button is clicked and the current variable is a factor variable.
    observeEvent(input$open_levels_input, {
      facet_var <- input$facet_var
      experiment_data <- experiment_data()
      experiment_name <- input$experiment_name

      req(experiment_name, facet_var)

      current_facet_var <- colData(experiment_data)[[facet_var]]

      if (is.factor(current_facet_var)) {
        facet_levels <- levels(current_facet_var)

        # Note: here we make sure we load with previous choice so the user
        # does not constantly need to start from scratch again.
        selected_groups <- assign_lists[[experiment_name]][[facet_var]]

        showModal(combModal(
          facet_levels = facet_levels,
          n_max_groups = length(facet_levels),
          selected_groups = selected_groups
        ))
      } else {
        showNotification("Can only group levels for factor variables", type = "message")
      }
    })

    # When OK button is pressed, save the settings, and remove the modal.
    observeEvent(input$ok, {
      experiment_name <- input$experiment_name
      facet_var <- input$facet_var
      comb_assignment <- input$comb_assignment

      req(experiment_name, facet_var, comb_assignment)

      if (FALSE) {
        # Here we could do checks on comb_assignment, e.g. check that not just
        # 1 group was selected.
        showNotification(
          "Something went wrong",
          type = "error"
        )
      } else {
        assign_lists[[experiment_name]][[facet_var]] <- comb_assignment
        removeModal()
      }
    })
  })
}
