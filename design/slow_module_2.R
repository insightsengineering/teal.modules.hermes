nrows <- 500000
ncols <- 10
set.seed(123)
mat <- matrix(
  data = rpois(n = nrows * ncols, lambda = 1),
  nrow = nrows,
  ncol = ncols,
  dimnames = list(paste0("ENSG", seq_len(nrows)), seq_len(ncols))
)

library(hermes)
object <- HermesDataFromMatrix(mat)
mae <- wrap_in_mae(object)

library(teal.modules.hermes)

ui <- function(id,
               datasets,
               mae_name) {
  ns <- NS(id)

  teal.devel::standard_layout(
    encoding = div(
      experimentSpecInput(ns("experiment"), datasets, mae_name),
      assaySpecInput(ns("assay")),
      geneSpecInput(ns("genes"), funs = list(None = NULL))
    ),
    output = plotOutput(ns("plot"))
  )
}

srv <- function(input,
                output,
                session,
                datasets,
                mae_name) {
  experiment <- experimentSpecServer(
    "experiment",
    datasets = datasets,
    mae_name = mae_name,
    name_annotation = NULL  # If you have a gene name column in your rowData, can specify here.
  )
  assay <- assaySpecServer("assay", experiment$assays)
  genes <- geneSpecServer("genes", list(None = NULL), gene_choices = experiment$genes)
  output$plot <- renderPlot({
    data <- experiment$data()
    assay <- assay()
    req(assay %in% SummarizedExperiment::assayNames(data))
    genes <- genes()$get_genes()
    req(length(genes) > 0)
    counts <- SummarizedExperiment::assay(data, assay)[genes, , drop = FALSE]
    image(counts)
  })
}

mae_name <- "MAE"
app <- init(
  data = teal_data(
    dataset(mae_name, mae)
  ),
  modules = modules(
    module(
      label = "Slow Module",
      server = srv,
      server_args = list(mae_name = mae_name),
      ui = ui,
      ui_args = list(mae_name = mae_name),
      filters = mae_name
    )
  ),
  header = tags$h1("Slow App")
)

shinyApp(app$ui, app$server)
