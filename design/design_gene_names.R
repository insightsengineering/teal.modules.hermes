library(teal.modules.hermes)

ui <- function(id,
               datasets) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
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
    output = textOutput(ns("result"))
  )
}

h_update_gene_selection2 <- function(session,
                                     inputId, # nolint
                                     selected,
                                     choice_data) {
  is_new_selected <- selected %in% choice_data$id
  is_removed <- !is_new_selected
  updateOptionalSelectInput(
    session,
    inputId = inputId,
    selected = selected[is_new_selected],
    choices = value_choices(
      data = choice_data,
      var_choices = "id",
      var_label = "name"
    )
  )
  n_removed <- sum(is_removed)
  if (n_removed > 0) {
    showNotification(paste(
      "Removed", n_removed, ifelse(n_removed > 1, "genes", "gene"),
      hermes::h_parens(hermes::h_short_list(selected[is_removed]))
    ))
  }
}

server <- function(input,
                   output,
                   session,
                   datasets) {
  gene_data <- reactive({
    mae <- datasets$get_data("MAE", filtered = TRUE)
    data <- data.frame(
      id = rownames(mae[[1]]),
      name = SummarizedExperiment::rowData(mae[[1]])$HGNC
    )
    data[order(data$name), ]
  })
  observeEvent(gene_data(), {
    gene_data <- gene_data()
    old_selected <- input$genes

    h_update_gene_selection2(
      session,
      inputId = "genes",
      selected = old_selected,
      choice_data = gene_data
    )
  })
  output$result <- renderText({
    input$genes
  })
}

my_app <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      module(
        label = "gene names design",
        server = server,
        ui = ui,
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
