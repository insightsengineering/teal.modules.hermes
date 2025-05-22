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

#' @keywords internal
#' @noRd
toggle_dropdown_deps <- function() {
  htmltools::htmlDependency(
    name = "teal-modules-hermes-toggle_dropdown",
    version = utils::packageVersion("teal.modules.hermes"),
    package = "teal.modules.hermes",
    src = "js",
    script = "dropdown.js"
  )
}
