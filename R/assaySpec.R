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
#' ui <- function(id,
#'                data) {
#'   ns <- NS(id)
#'   teal.widgets::standard_layout(
#'     encoding = div(
#'       experimentSpecInput(
#'         ns("experiment"),
#'         data,
#'         "MAE"
#'       ),
#'       assaySpecInput(
#'         ns("assay"),
#'         label_assays = "Please choose assay"
#'       )
#'     ),
#'     output = textOutput(ns("result"))
#'   )
#' }
#'
#' server <- function(id, data, filter_panel_api) {
#'   moduleServer(id, module = function(input, output, session) {
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
#'   mae <- hermes::multi_assay_experiment
#'   mae_name <- "MAE"
#'   mae_data <- dataset(mae_name, mae)
#'   data <- teal_data(mae_data)
#'   app <- init(
#'     data = data,
#'     modules = modules(
#'       module(
#'         label = "assaySpec example",
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
assaySpecServer <- function(id,
                            assays,
                            exclude_assays = character()) {
  assert_string(id)
  assert_reactive(assays)
  assert_character(exclude_assays, any.missing = FALSE)

  moduleServer(id, function(input, output, session) {

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
