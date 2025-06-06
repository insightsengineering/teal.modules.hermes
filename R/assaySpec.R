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
assaySpecInput <- function(inputId, # nolint
                           label_assays = "Select Assay") {
  assert_string(inputId)
  assert_string(label_assays, min.chars = 1L)

  ns <- NS(inputId)
  tags$div(
    selectizeInput(
      inputId = ns("name"),
      label = label_assays,
      choices = character(0),
      options = list(
        placeholder = "- Nothing selected -"
      )
    )
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
#' ui <- function(id) {
#'   ns <- NS(id)
#'   teal.widgets::standard_layout(
#'     encoding = uiOutput(ns("encoding_ui")),
#'     output = textOutput(ns("result"))
#'   )
#' }
#'
#' server <- function(id, data, filter_panel_api) {
#'   moduleServer(id, module = function(input, output, session) {
#'     output$encoding_ui <- renderUI({
#'       tags$div(
#'         experimentSpecInput(session$ns("experiment"), data, "MAE"),
#'         assaySpecInput(
#'           session$ns("assay"),
#'           label_assays = "Please choose assay"
#'         )
#'       )
#'     })
#'     experiment <- experimentSpecServer(
#'       id = "experiment",
#'       data = data,
#'       filter_panel_api = filter_panel_api,
#'       mae_name = "MAE"
#'     )
#'     assay <- assaySpecServer(
#'       "assay",
#'       experiment$assays,
#'       exclude_assays = c("counts", "cpm", "tpm", "bla")
#'     )
#'     output$result <- renderPrint({
#'       assay()
#'     })
#'   })
#' }
#'
#' my_app <- function() {
#'   data <- teal_data(MAE = hermes::multi_assay_experiment)
#'   app <- init(
#'     data = data,
#'     modules = modules(
#'       module(
#'         label = "assaySpec example",
#'         server = server,
#'         ui = ui,
#'         datanames = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#' if (interactive()) {
#'   my_app()
#' }
assaySpecServer <- function(id, # nolint
                            assays,
                            exclude_assays = character()) {
  assert_string(id)
  assert_reactive(assays)
  assert_character(exclude_assays, any.missing = FALSE)

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.hermes")
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
      if (length(remaining_assays) == 0) {
        remaining_assays <- character(0)
      }
      remaining_assays
    })

    observeEvent(choices(), {
      choices <- choices()
      updateSelectizeInput(session, "name", choices = choices)
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
