#' Additional Assertions for `checkmate`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional assertion functions which can be used together with
#' the `checkmate` functions. These are described in individual help pages
#' linked below.
#'
#' @return Depending on the function prefix.
#' - `assert_` functions return the object invisibly if successful, and otherwise
#'   throw an error message.
#' - `check_` functions return `TRUE` if successful, otherwise a string with the
#'   error message.
#' - `test_` functions just return `TRUE` or `FALSE`.
#'
#' @seealso [check_tag()]
#'
#' @name assertions
#' @import checkmate
NULL

# check_tag ----

#' Check for Shiny Tag
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Check whether `x` is a shiny tag.
#'
#' @inheritParams assertion_arguments
#' @seealso [`assertions`] for more details.
#'
#' @export
#'
#' @examples
#' check_tag("bla")
#' check_tag(NULL, null.ok = TRUE)
check_tag <- function(x, null.ok = FALSE) {
  assert_flag(null.ok)
  ok <- (null.ok && test_null(x)) || test_class(x, "shiny.tag")
  if (!ok)
    return("Must be a 'shiny.tag' or NULL")
  return(TRUE)
}

#' @rdname check_tag
#' @inheritParams assertion_arguments
#' @export
assert_tag <- makeAssertionFunction(check_tag)

#' @rdname check_tag
#' @export
test_tag <- makeTestFunction(check_tag)
