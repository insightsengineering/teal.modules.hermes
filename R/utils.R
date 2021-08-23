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


# Don't export, see explanation in `teal::include_css_files`
#' @inherit teal::include_css_files
include_css_files <- function(pattern = "*") { # nolint # nousage
  css_files <- list.files(
    system.file("css", package = "teal.modules.hermes", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  return(singleton(lapply(css_files, includeCSS)))
}


# Don't export, see explanation in `teal::include_js_files`
#' @inherit teal::include_js_files
include_js_files <- function(pattern = "*") { # nolint # nousage
  js_files <- list.files(
    system.file("js", package = "teal.modules.hermes", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  return(singleton(lapply(js_files, includeScript)))
}
