#' Teal Module for RNA-seq Quality Control
#'
#' This module conducts quality control on a SummarizedExperiment input
#' for RNA-seq gene expression analysis.
#'
#' @inheritParams module_arguments
#'
#' @return Shiny module to be used in the teal app.
#'
#' @export
#'
#' @examples
#' library(hermes)
#' utils::data("multi_assay_experiment", package = "hermes")
#' for (i in seq_along(multi_assay_experiment)) {
#'   multi_assay_experiment[[i]] <- hermes::HermesData(multi_assay_experiment[[i]])
#' }
#' mae_data <- dataset("MAE", multi_assay_experiment)
#' data <- teal_data(mae_data)
#' app <- init(
#'   data = data,
#'   modules = root_modules(
#'     static = {
#'       tm_g_scatterplot(
#'         label = "scatterplot",
#'         mae_name = "MAE"
#'       )
#'     }
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_quality <- function(label,
                             mae_name,
                             pre_output = NULL,
                             post_output = NULL) {
  assert_string(label)
  assert_string(mae_name)
  assert_tag(pre_output, null.ok = TRUE)
  assert_tag(post_output, null.ok = TRUE)
  
  module(
    label = label,
    server = srv_g_scatterplot,
    server_args = list(
      mae_name = mae_name
    ),
    ui = ui_g_scatterplot,
    ui_args = list(
      mae_name = mae_name,
      pre_output = pre_output,
      post_output = post_output
    ),
    filters = "all"
  )
}
