#' Module Input for Gene Signature Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the gene signature specification.
#'
#' @inheritParams module_arguments
#' @param funs (named `list`)\cr names of this list will be used for the function
#'   selection drop down menu.
#' @param label_genes (`string`)\cr label for the gene selection.
#' @param label_funs (`string`)\cr label for the function selection.
#' @param label_text_button (`string`)\cr label for the text input button.
#' @param label_lock_button (`string`)\cr label for the lock button.
#'
#' @return The UI part.
#' @seealso [geneSpecServer()] for the module server and a complete example.
#' @export
#'
#' @examples
#' geneSpecInput("my_genes", list(mean = colMeans), label_funs = "Please select function")
geneSpecInput <- function(inputId,
                          funs,
                          label_genes = "Select gene(s)",
                          label_funs = "Select gene summary",
                          label_text_button = "Enter list of genes",
                          label_lock_button = "Lock gene selection") {
  assert_string(inputId)
  assert_list(funs, names = "unique", min.len = 1L)
  assert_string(label_genes)
  assert_string(label_funs)
  assert_string(label_text_button)
  assert_string(label_lock_button)

  ns <- NS(inputId)
  tagList(
    div(
      class = "row",
      div(
        class = "col-sm-8",
        tags$label(
          class = "control-label",
          label_genes
        )
      ),
      div(
        class = "col-sm-4",
        actionButton(
          ns("text_button"),
          span(icon("font fa-border")),
          title = label_text_button,
          class = "pull-right list-genes"
        ),
        div(
          class = "pull-right",
          title = label_lock_button,
          shinyWidgets::prettyToggle(
            ns("lock_button"),
            value = FALSE,
            label_on = NULL,
            label_off = NULL,
            status_on = "default",
            status_off = "default",
            outline = FALSE,
            plain = TRUE,
            icon_on = icon("lock fa-border"),
            icon_off = icon("unlock-alt fa-border"),
            animation = "pulse"
          )
        ),
        include_css_files(pattern = "*")
      )
    ),
    div(
      class = "custom-select-input",
      optionalSelectInput(
        ns("genes"),
        label = NULL,
        choices = "",
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE
        )
      )
    ),
    conditionalPanel(
      condition = "input.genes.length > 1",
      ns = ns,
      selectInput(
        ns("fun_name"),
        label_funs,
        names(funs)
      )
    )
  )
}

#' Helper Function to Update Gene Selection
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function takes the intersection of `selected` and
#' `choices` for genes and updates the `inputId` accordingly. It then
#' shows a notification if not all `selected` genes were available.
#'
#' @inheritParams module_arguments
#' @inheritParams teal::updateOptionalSelectInput
#'
#' @export
h_update_gene_selection <- function(session,
                                    inputId,
                                    selected,
                                    choices_data) {
  assert_data_frame(choices_data, col.names = "unique", ncols = 2L)
  assert_subset(c("gene_id", "gene_name"), names(choices_data))

  is_new_selected <- selected %in% choices_data$gene_id
  is_removed <- !is_new_selected
  updateOptionalSelectInput(
    session,
    inputId = inputId,
    selected = selected[is_new_selected],
    choices = setNames(choices_data$gene_id, choices_data$gene_name)
  )
  n_removed <- sum(is_removed)
  if (n_removed > 0) {
    showNotification(paste(
      "Removed", n_removed, ifelse(n_removed > 1, "genes", "gene"),
      hermes::h_parens(hermes::h_short_list(selected[is_removed]))
    ))
  }
}

#' Module Server for Gene Signature Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the gene signature specification.
#'
#' @inheritParams module_arguments
#' @param funs (static named `list`)\cr names of this list will be used for the function
#'   selection drop down menu.
#' @param gene_choices (reactive `data.frame`)\cr returns the possible gene choices to
#'   populate in the UI, a `data.frame` with columns `gene_id` and `gene_name`.
#' @param label_modal_title (`string`)\cr title for the dialog that asks for the text input.
#' @param label_modal_footer (`character`)\cr lines of text to use for the footer of the dialog.
#'
#' @return Reactive [`hermes::GeneSpec`] which can be used as input for the relevant
#'   `hermes` functions.
#' @seealso [geneSpecInput()] for the module UI.
#'
#' @export
#'
#' @examples
#' ui <- function(id,
#'                datasets,
#'                funs) {
#'   ns <- NS(id)
#'   teal.devel::standard_layout(
#'     encoding = div(
#'       geneSpecInput(
#'         ns("my_genes"),
#'         funs = funs,
#'         label_funs = "Please select function"
#'       )
#'     ),
#'     output = textOutput(ns("result"))
#'   )
#' }
#' server <- function(input,
#'                    output,
#'                    session,
#'                    datasets,
#'                    funs) {
#'   gene_choices <- reactive({
#'     mae <- datasets$get_data("MAE", filtered = TRUE)
#'     object <- mae[[1]]
#'     gene_ids <- rownames(object)
#'     gene_names <- SummarizedExperiment::rowData(object)$HGNC
#'     data.frame(
#'       gene_id = gene_ids,
#'       gene_name = gene_names
#'     )[order(gene_names), , drop = FALSE]
#'   })
#'   gene_spec <- geneSpecServer(
#'     "my_genes",
#'     funs = funs,
#'     gene_choices = gene_choices
#'   )
#'   output$result <- renderText({
#'     validate_gene_spec(
#'       gene_spec(),
#'       gene_choices()
#'     )
#'     gene_spec <- gene_spec()
#'     gene_spec$get_label()
#'   })
#' }
#' funs <- list(mean = colMeans)
#' my_app <- function() {
#'   mae <- hermes::multi_assay_experiment
#'   mae_data <- dataset("MAE", mae)
#'   data <- teal_data(mae_data)
#'   app <- init(
#'     data = data,
#'     modules = root_modules(
#'       module(
#'         label = "GeneSpec example",
#'         server = server,
#'         server_args = list(funs = funs),
#'         ui = ui,
#'         ui_args = list(funs = funs),
#'         filters = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#' if (interactive()) {
#'   my_app()
#' }
geneSpecServer <- function(inputId,
                           funs,
                           gene_choices,
                           label_modal_title = "Enter list of genes",
                           label_modal_footer = c(
                             "Please enter a comma-separated list of gene IDs.",
                             "(Note that genes not included in current choices will be removed)"
                           )) {
  assert_string(inputId)
  assert_list(funs, names = "unique", min.len = 1L)
  assert_reactive(gene_choices)
  assert_string(label_modal_title)
  assert_character(label_modal_footer)

  moduleServer(inputId, function(input, output, session) {
    # The `reactiveValues` object for storing current gene text input.
    parsed_genes <- reactiveVal(NULL, label = "Parsed genes")

    # If the parsed genes are entered via text, update gene selection.
    observeEvent(parsed_genes(), ignoreNULL = TRUE, {
      gene_choices <- gene_choices()
      parsed_genes <- parsed_genes()

      h_update_gene_selection(
        session,
        inputId = "genes",
        selected = parsed_genes,
        choices = gene_choices
      )
    })

    # When
    # 1) the gene choices are recomputed,
    # 2) the lock is pressed and then switched off,
    # then update gene selection.
    observeEvent(list(gene_choices(), input$lock_button), {
      gene_choices <- gene_choices()
      lock_button <- input$lock_button
      old_selected <- input$genes

      if (isFALSE(lock_button)) {
        h_update_gene_selection(
          session,
          inputId = "genes",
          selected = old_selected,
          choices = gene_choices
        )
      }
    })

    # Return the UI for a modal dialog with gene text input, showing examples.
    dataModal <- function(example_list) {
      modalDialog(
        textInput(
          session$ns("gene_text"),
          label = label_modal_title,
          placeholder = example_list
        ),
        do.call("span", as.list(label_modal_footer)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("ok_button"), "OK")
        )
      )
    }

    # Show modal when the text button is clicked.
    observeEvent(input$text_button, {
      gene_choices <- gene_choices()
      example_list <- hermes::h_short_list(gene_choices)
      showModal(dataModal(example_list))
    })

    # When OK button is pressed, attempt to parse the genes from the text.
    # Remove the modal and display notification message.
    observeEvent(input$ok_button, {
      gene_text <- input$gene_text

      if (!nzchar(gene_text)) {
        showNotification(
          "Please enter at least one full gene ID.",
          type = "error"
        )
      } else {
        parse_result <- h_extract_words(gene_text)
        showNotification(paste(
          "Received", length(parse_result), "genes from text input",
          hermes::h_parens(hermes::h_short_list(parse_result))
        ))
        parsed_genes(parse_result)
        removeModal()
      }
    })

    reactive({
      hermes::gene_spec(
        genes = input$genes,
        fun = funs[[input$fun_name]],
        fun_name = input$fun_name
      )
    })
  })
}

#' Validation of Gene Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This validation function checks that a given [`hermes::GeneSpec`] has at least
#' one gene selected and that all genes are included in possible choices.
#'
#' @param gene_spec (`GeneSpec`)\cr gene specification.
#' @param gene_choices (`data.frame`)\cr all possible gene choices.
#'
#' @export
validate_gene_spec <- function(gene_spec,
                               gene_choices) {
  assert_r6(gene_spec, "GeneSpec")
  assert_data_frame(gene_choices, col.names = "unique", ncols = 2L)
  assert_subset(c("gene_id", "gene_name"), names(gene_choices))

  validate(need(
    !is.null(gene_spec$get_genes()),
    "please select at least one gene"
  ))
  genes_not_included <- setdiff(gene_spec$get_genes(), gene_choices$gene_id)
  n_not_incl <- length(genes_not_included)
  validate(need(
    identical(n_not_incl, 0L),
    paste(
      n_not_incl,
      ifelse(n_not_incl > 1, "genes", "gene"),
      hermes::h_parens(hermes::h_short_list(genes_not_included)),
      "not included, please unlock or change filters"
    )
  ))
}
