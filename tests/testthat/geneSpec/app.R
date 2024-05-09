library(teal.modules.hermes)

ui <- function(id, funs) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    encoding = tags$div(
      geneSpecInput(
        ns("my_genes"),
        funs = funs,
        label_funs = "Please select function"
      )
    ),
    output = textOutput(ns("result"))
  )
}

server <- function(id,
                   data,
                   funs) {
  moduleServer(id, function(input, output, session) {
    gene_choices <- reactive({
      mae <- data()[["MAE"]]
      object <- mae[[1]]
      gene_ids <- rownames(object)
      gene_names <- SummarizedExperiment::rowData(object)$symbol
      gene_data <- data.frame(
        id = gene_ids,
        name = gene_names
      )
      gene_data[order(gene_data$name), ]
    })
    gene_spec <- geneSpecServer(
      "my_genes",
      funs = funs,
      gene_choices = gene_choices
    )
    output$result <- renderText({
      validate_gene_spec(
        gene_spec(),
        gene_choices()$id
      )
      gene_spec <- gene_spec()
      gene_spec$get_label()
    })
  })
}

funs <- list(mean = colMeans)
my_app <- function() {
  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal::init(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "GeneSpec example",
        server = server,
        server_args = list(funs = funs),
        ui = ui,
        ui_args = list(funs = funs),
        datanames = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}

my_app()
