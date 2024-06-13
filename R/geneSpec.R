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
#' @param label_select_all_button (`string`)\cr label for the selecting all genes button.
#' @param label_select_none_button (`string`)\cr label for the selecting no genes button.
#' @param max_options (`count`)\cr maximum number of gene options rendering and selected via
#'   "Select All".
#' @param max_selected (`count`)\cr maximum number of genes which can be selected.
#'
#' @return The UI part.
#' @seealso [geneSpecServer()] for the module server and a complete example.
#' @export
#'
#' @examples
#' geneSpecInput("my_genes", list(mean = colMeans), label_funs = "Please select function")
geneSpecInput <- function(inputId, # nolint
                          funs,
                          label_genes = "Select Gene(s)",
                          label_funs = "Select Gene Summary",
                          label_text_button = "Enter list of genes",
                          label_lock_button = "Lock gene selection (so that it does not get updated when filtering)",
                          label_select_all_button = paste0("Select All Genes (first ", max_options, ")"),
                          label_select_none_button = "Select None",
                          max_options = 200L,
                          max_selected = max_options) {
  assert_string(inputId)
  assert_list(funs, names = "unique", min.len = 1L)
  assert_string(label_genes)
  assert_string(label_funs)
  assert_string(label_text_button)
  assert_string(label_lock_button)
  assert_string(label_select_all_button)
  assert_string(label_select_none_button)
  assert_count(max_options, positive = TRUE)
  assert_count(max_selected, positive = TRUE)

  ns <- NS(inputId)
  tagList(
    include_css_files(pattern = "*"),
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-8",
        tags$label(
          class = "control-label",
          label_genes
        )
      ),
      tags$div(
        class = "col-sm-2",
        actionButton(
          ns("select_none_button"),
          tags$span(icon("remove-circle", lib = "glyphicon")),
          title = label_select_none_button,
          class = "pull-right list-genes"
        ),
        actionButton(
          ns("select_all_button"),
          tags$span(icon("ok-circle", lib = "glyphicon")),
          title = label_select_all_button,
          class = "pull-right list-genes"
        )
      ),
      tags$div(
        class = "col-sm-2",
        actionButton(
          ns("text_button"),
          tags$span(icon("fas fa-font")),
          title = label_text_button,
          class = "pull-right list-genes"
        ),
        tags$div(
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
            icon_on = icon("fas fa-lock"),
            icon_off = icon("fas fa-lock-open"),
            animation = "pulse"
          )
        )
      )
    ),
    tags$div(
      class = "custom-select-input",
      selectizeInput(
        ns("genes"),
        label = NULL,
        choices = "",
        multiple = TRUE,
        selected = 1,
        options = list(
          placeholder = "- Nothing selected -",
          render = I("{
          option: function(item, escape) {
              return '<div> <span style=\"font-size: inherit;\">' + item.label + '</div>' +
                ' <span style=\"color: #808080; font-size: xx-small;\" >' + item.value + '</div> </div>'
            }
          }"),
          searchField = c("value", "label"),
          maxOptions = max_options,
          maxItems = max_selected
        )
      )
    ),
    conditionalPanel(
      condition = "input.genes && input.genes.length > 1",
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
#' @param session (`ShinySession`)\cr the session object.
#' @param selected (`character`)\cr currently selected gene IDs.
#' @param choices (`data.frame`)\cr containing `id` and `name` columns of the
#'   new choices.
#'
#' @export
h_update_gene_selection <- function(session,
                                    inputId, # nolint
                                    selected,
                                    choices) {
  is_new_selected <- selected %in% choices$id
  is_removed <- !is_new_selected
  updateSelectizeInput(
    session = session,
    inputId = inputId,
    selected = selected[is_new_selected],
    choices = stats::setNames(choices$id, choices$name),
    server = TRUE
  )

  n_removed <- sum(is_removed)
  if (n_removed > 0) {
    showNotification(paste(
      "Removed", n_removed, ifelse(n_removed > 1, "genes", "gene"),
      hermes::h_parens(hermes::h_short_list(selected[is_removed]))
    ))
  }
}

#' Helper Function to Parse Genes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function takes a vector of `words` and tries to match them
#' with the `id` and `name` columns of possible gene choices.
#'
#' @param words (`character`)\cr containing gene IDs or names.
#' @inheritParams h_update_gene_selection
#' @return The subset of `choices` which matches `words` in ID or name.
#'
#' @export
#' @examples
#' h_parse_genes(
#'   c("a", "2535"),
#'   data.frame(id = as.character(2533:2537), name = letters[1:5])
#' )
h_parse_genes <- function(words, choices) {
  assert_character(words, min.len = 1L)
  assert_data_frame(choices, types = "character")
  assert_set_equal(names(choices), c("id", "name"))

  id_matches <- choices$id %in% words
  name_matches <- choices$name %in% words
  has_match <- id_matches | name_matches
  choices[has_match, , drop = FALSE]
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
#'   populate in the UI, as a `data.frame` with columns `id` and `name`.
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
#' ui <- function(id, funs) {
#'   ns <- NS(id)
#'   teal.widgets::standard_layout(
#'     encoding = tags$div(
#'       geneSpecInput(
#'         ns("my_genes"),
#'         funs = funs,
#'         label_funs = "Please select function"
#'       )
#'     ),
#'     output = textOutput(ns("result"))
#'   )
#' }
#' server <- function(id,
#'                    data,
#'                    funs) {
#'   checkmate::assert_class(data, "reactive")
#'   checkmate::assert_class(shiny::isolate(data()), "teal_data")
#'   moduleServer(id, function(input, output, session) {
#'     gene_choices <- reactive({
#'       mae <- data()[["MAE"]]
#'       object <- mae[[1]]
#'       gene_ids <- rownames(object)
#'       gene_names <- SummarizedExperiment::rowData(object)$symbol
#'       gene_data <- data.frame(
#'         id = gene_ids,
#'         name = gene_names
#'       )
#'       gene_data[order(gene_data$name), ]
#'     })
#'     gene_spec <- geneSpecServer(
#'       "my_genes",
#'       funs = funs,
#'       gene_choices = gene_choices
#'     )
#'     output$result <- renderText({
#'       validate_gene_spec(
#'         gene_spec(),
#'         gene_choices()$id
#'       )
#'       gene_spec <- gene_spec()
#'       gene_spec$get_label()
#'     })
#'   })
#' }
#' funs <- list(mean = colMeans)
#' my_app <- function() {
#'   data <- teal_data(MAE = hermes::multi_assay_experiment)
#'   app <- init(
#'     data = data,
#'     modules = modules(
#'       module(
#'         label = "GeneSpec example",
#'         server = server,
#'         server_args = list(funs = funs),
#'         ui = ui,
#'         ui_args = list(funs = funs),
#'         datanames = "all"
#'       )
#'     )
#'   )
#'   shinyApp(app$ui, app$server)
#' }
#' if (interactive()) {
#'   my_app()
#' }
geneSpecServer <- function(id, # nolint
                           funs,
                           gene_choices,
                           label_modal_title = "Enter list of genes",
                           label_modal_footer = c(
                             "Please enter a comma-separated list of gene IDs and/or names.",
                             "(Note that genes not included in current choices will be removed)"
                           )) {
  assert_string(id)
  assert_list(funs, names = "unique", min.len = 1L)
  assert_reactive(gene_choices)
  assert_string(label_modal_title)
  assert_character(label_modal_footer)

  moduleServer(id, function(input, output, session) {
    tmh_track_shiny_input_changes(input)
    # The `reactiveValues` object for storing current gene text input.
    # This will also be a data frame with id and name columns.
    parsed_genes <- reactiveVal(NULL, label = "Parsed genes")

    # If the parsed genes are entered via text, update gene selection.
    observeEvent(parsed_genes(), ignoreNULL = TRUE, {
      gene_choices <- gene_choices()
      parsed_genes <- parsed_genes()

      h_update_gene_selection(
        session,
        inputId = "genes",
        selected = parsed_genes$id,
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

    # When the Select All button is pressed and not locked, select all genes.
    observeEvent(input$select_all_button, {
      gene_choices <- gene_choices()
      lock_button <- input$lock_button

      if (isFALSE(lock_button)) {
        h_update_gene_selection(
          session,
          inputId = "genes",
          selected = gene_choices$id,
          choices = gene_choices
        )
      } else {
        showNotification(
          "Please unlock if you would like to select all genes",
          type = "warning"
        )
      }
    })

    # When the Select None button is pressed and not locked, select none.
    observeEvent(input$select_none_button, {
      gene_choices <- gene_choices()
      lock_button <- input$lock_button

      if (isFALSE(lock_button)) {
        h_update_gene_selection(
          session,
          inputId = "genes",
          selected = character(),
          choices = gene_choices
        )
      } else {
        showNotification(
          "Please unlock if you would like to select none",
          type = "warning"
        )
      }
    })

    # Return the UI for a modal dialog with gene text input, showing examples.
    dataModal <- function(example_list) { # nolint
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
      example_list <- hermes::h_short_list(utils::head(setdiff(gene_choices$name, "")))
      showModal(dataModal(example_list))
    })

    # When OK button is pressed, attempt to parse the genes from the text.
    # This can be IDs and/or names of genes.
    # Remove the modal and display notification message.
    observeEvent(input$ok_button, {
      gene_text <- input$gene_text
      gene_choices <- gene_choices()

      if (!nzchar(gene_text)) {
        showNotification(
          "Please enter at least one full gene ID.",
          type = "error"
        )
      } else {
        words <- h_extract_words(gene_text)
        parse_result <- h_parse_genes(words, choices = gene_choices)
        showNotification(paste(
          "Parsed total", nrow(parse_result), "genes from", length(words), "words"
        ))
        parsed_genes(parse_result)
        removeModal()
      }
    })

    # When the gene choice is updated, then also set the names
    # correctly by looking up in current choices.
    named_genes <- eventReactive(input$genes, ignoreNULL = FALSE, {
      genes <- input$genes
      gene_choices <- gene_choices()
      ret <- if (!is.null(genes)) {
        which_id <- match(genes, gene_choices$id)
        gene_names <- gene_choices$name[which_id]
        stats::setNames(genes, gene_names)
      } else {
        NULL
      }
      ret
    })

    reactive({
      hermes::gene_spec(
        genes = named_genes(),
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
