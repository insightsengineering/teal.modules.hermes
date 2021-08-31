library(teal.modules.hermes)

ui <- function(id,
               datasets,
               funs) {
  ns <- NS(id)
  teal.devel::standard_layout(
    encoding = div(
      geneSpecInput(
        ns("my_genes"),
        funs = funs,
        label_funs = "Please select function"
      )
    ),
    output = textOutput(ns("result"))
  )
}
server <- function(input,
                   output,
                   session,
                   datasets,
                   funs) {
  gene_choices <- reactive({
    mae <- datasets$get_data("MAE", filtered = TRUE)
    rownames(mae[[1]])
  })
  gene_spec <- geneSpecServer(
    "my_genes",
    funs = funs,
    gene_choices = gene_choices
  )
  output$result <- renderText({
    validate_gene_spec(
      gene_spec(),
      gene_choices()
    )
    gene_spec <- gene_spec()
    gene_spec$get_label()
  })
}
funs <- list(mean = colMeans)
my_app <- function() {
  mae <- hermes::multi_assay_experiment
  mae_data <- dataset("MAE", mae)
  data <- teal_data(mae_data)
  app <- init(
    data = data,
    modules = root_modules(
      module(
        label = "GeneSpec example",
        server = server,
        server_args = list(funs = funs),
        ui = ui,
        ui_args = list(funs = funs),
        filters = "all"
      )
    )
  )
  shinyApp(app$ui, app$server)
}
my_app()
