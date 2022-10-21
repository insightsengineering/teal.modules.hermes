library(shinytest2)

module_ns_shiny2 <- function(app) {
  source <- app$get_html("html", outer_html = TRUE)
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(source), css = ".teal_module"),
    "id"
  )
  NS(module_id)
}

ns2 <- NS("teal-main_ui-filter_panel")
