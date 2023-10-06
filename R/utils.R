#' Checking for Empty String
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This predicate function is helpful for functions where arguments could
#' not yet be initialized from the teal module.
#'
#' @param x object to check.
#'
#' @return Flag whether `x` is identical to an empty string, i.e. `""`.
#' @export
#'
#' @examples
#' is_blank("")
#' is_blank(" ")
is_blank <- function(x) {
  identical(x, "")
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

#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included
#'
#' @return HTML code that includes `CSS` files
#' @keywords internal
include_css_files <- function(pattern = "*") { # nolint
  css_files <- list.files(
    system.file("css", package = "teal.modules.hermes", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  return(shiny::singleton(shiny::tags$head(lapply(css_files, includeCSS))))
}

#' Include `JS` files from `/inst/js/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) pattern of files to be included, passed to `system.file`
#' @param except (`character`) vector of basename filenames to be excluded
#'
#' @return HTML code that includes `JS` files
#' @keywords internal
include_js_files <- function(pattern = "*") { # nolint
  js_files <- list.files(
    system.file("js", package = "teal.modules.hermes", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  return(singleton(lapply(js_files, includeScript)))
}

#' Template function to generate reporter card for `teal.modules.hermes`
#' @param title (`character(1)`) title of the card (unless overwritten by label)
#' @param label (`character(1)`) label provided by the user when adding the card
#' @param description (`character(1)`) optional additional description
#' @param filter_panel_api (`FilterPanelAPI`) object with API that allows the generation
#' of the filter state in the report
#'
#' @return (`TealReportCard`) populated with a title, description and filter state
#'
#' @keywords internal
card_template <- function(title, label, description = NULL, filter_panel_api) {
  assert_string(title)
  assert_string(label)
  assert_string(description, null.ok = TRUE)
  assert_class(filter_panel_api, classes = "FilterPanelAPI")

  card <- teal::TealReportCard$new()
  title <- if (label == "") title else label
  card$set_name(title)
  card$append_text(title, "header2")
  if (!is.null(description)) {
    card$append_text(description, "header3")
  }
  card$append_fs(filter_panel_api$get_filter_state())
  card
}
