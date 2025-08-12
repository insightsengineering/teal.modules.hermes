#' Checking for Empty String
#'
#' @description
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
#' @description
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
