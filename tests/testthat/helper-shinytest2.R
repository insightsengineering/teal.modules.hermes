library(shinytest2)

module_ns_shiny2 <- function(app) {
  source <- app$get_html("html", outer_html = TRUE)
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(source), css = ".teal_module"),
    "id"
  )
  NS(paste0(module_id, "-module"))
}

ns2 <- NS("teal-main_ui-filter_panel")
default_app_seed <- 123

# expect_select_screenshot ----
shinytest2::AppDriver$set(
  which = "public",
  name = "expect_select_screenshot",
  value = function(selector, ...) {
    self$expect_screenshot(selector = paste0("#", selector), ...)
  },
  overwrite = TRUE
)
