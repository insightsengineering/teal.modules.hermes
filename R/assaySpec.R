#' Module Input for Assay Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the assay specification.
#'
#' @inheritParams module_arguments
#' @param label_assays (`string`)\cr label for the assay selection.
#'
#' @return The UI part.
#' @seealso [assaySpecServer()] for the module server and a complete example.
#' @export
assaySpecInput <- function(inputId,
                           label_assays = "Select Assay") {
  assert_string(inputId)
  assert_string(label_assays, min.chars = 1L)

  ns <- NS(inputId)
  selectInput(
    inputId = ns("name"),
    label = label_assays,
    choices = ""
  )
}

#' Module Server for Assay Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the assay specification.
#'
#' @inheritParams module_arguments
#' @param assays (reactive `character`)\cr available assays in the currently selected experiment.
#' @return The chosen assay as a reactive string.
#'
#' @seealso [assaySpecInput()] for the module UI.
#'
#' @export
#'
#' @examples

assaySpecServer <- function(inputId,
                            assays,
                            exclude_assays = character()) {
  assert_string(inputId)
  assert_reactive(assays)
  assert_character(exclude_assays, any.missing = FALSE)

  moduleServer(inputId, function(input, output, session) {

    # When the assay names change, update the choices for assay.
    choices <- reactive({
      assays <- assays()
      remaining_assays <- setdiff(
        assays,
        exclude_assays
      )
      removed_assays <- setdiff(assays, remaining_assays)
      if (length(removed_assays) > 0) {
        showNotification(type = "warning", paste(
          "Excluded", ifelse(length(removed_assays) > 1, "assays", "assay"),
          hermes::h_short_list(removed_assays), "as per app specifications"
        ))
      }
      remaining_assays
    })

    observeEvent(choices(), {
      choices <- choices()
      updateSelectInput(
        session,
        "name",
        choices = choices
      )
    })

    reactive({
      choices <- choices()
      validate(need(
        length(choices) > 0,
        "No assays eligible for this experiment, please make sure to add normalized assays"
      ))
      input$name
    })
  })
}
