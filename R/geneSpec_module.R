#' Module Input for Gene Signature Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the input for the gene signature specification.
#'
#' @inheritParams argument_convention
#' @param funs (named `list`)\cr names of this list will be used for the function
#'   selection drop down menu.
#' @param label_genes (`string`)\cr label for the gene selection.
#' @param label_funs (`string`)\cr label for the function selection.
#' @param label_text_button (`string`)\cr label for the text input button.
#' @param label_lock_button (`string`)\cr label for the lock button.
#'
#' @return The UI part.
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
        multiple = TRUE
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
#' @inheritParams argument_convention
#' @inheritParams teal::updateOptionalSelectInput
#'
#' @export
h_update_gene_selection <- function(session,
                                    inputId,
                                    selected,
                                    choices) {
  new_selected <- intersect(selected, choices)
  removed <- setdiff(selected, new_selected)
  updateOptionalSelectInput(
    session,
    inputId = inputId,
    selected = new_selected,
    choices = choices
  )
  n_removed <- length(removed)
  if (n_removed > 0) {
    showNotification(paste(
      "Removed", n_removed, ifelse(n_removed > 1, "genes", "gene"),
      hermes::parens(hermes::h_short_list(removed))
    ))
  }
}

#' Helper Function to Extract Words
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function extracts words from a string. Here words are defined
#' as containing lower or upper case letters, colons and dots. All other
#' characters are considered separators.
#'
#' @param x (`string`)\cr input.
#'
#' @return Character vector with the extracted words.
#' @export
#'
#' @examples
#' h_extract_words("a, b, , c, 234; 34562 - GeneID:bla")
#' h_extract_words("GeneID:1820, sdf.393; 32596")
h_extract_words <- function(x) {
  assert_string(x, min.chars = 1L)
  stringr::str_extract_all(
    x,
    "[a-zA-Z0-9:\\.]+"
  )[[1]]
}

#' Module Server for Gene Signature Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This defines the server part for the gene signature specification.
#'
#' @inheritParams argument_convention
#' @param funs (static named `list`)\cr names of this list will be used for the function
#'   selection drop down menu.
#' @param gene_choices (reactive `character`)\cr returns the possible gene choices to
#'   populate in the UI.
#'
#' @return Reactive [`hermes::GeneSpec`] which can be used as input for the relevant
#'   `hermes` functions.
#' @export
#'
#' @examples
#' funs <- list(mean = colMeans)
#' ui <- sidebarLayout(
#'   sidebarPanel(geneSpecInput(
#'     "my_genes",
#'     funs = funs,
#'     label_funs = "Please select function"
#'   )),
#'   mainPanel(textOutput("result"))
#' )
#' server <- function(input, output, session) {
#'   gene_choices <- reactive({letters})
#'   gene_spec <- geneSpecServer(
#'     "my_genes",
#'     funs = funs,
#'     gene_choices = gene_choices
#'   )
#'   output$result <- renderText({
#'     gene_spec <- gene_spec()
#'     gene_spec$get_label()
#'   })
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
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

      if (!lock_button) {
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
#' @param gene_choices (`character`)\cr all possible gene choices.
#'
#' @export
validate_gene_spec <- function(gene_spec,
                               gene_choices) {
  assert_r6(gene_spec, "GeneSpec")
  assert_character(gene_choices)

  validate(need(
    !is.null(gene_spec$get_genes()),
    "please select at least one gene"
  ))
  genes_not_included <- setdiff(gene_spec$get_genes(), gene_choices)
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
