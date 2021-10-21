nrows <- 5000
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
app <- init(
  data = teal_data(
    mae_dataset("MAE", mae)
  ),
  modules = root_modules(
    tm_g_boxplot(
      label = "boxplot",
      mae_name = "MAE",
      exclude_assays = character()
    )
  ),
  header = tags$h1("Example App with DataSetDB Data with Delayed Data Loading")
)

shinyApp(app$ui, app$server)
