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
#' @seealso [assert_tag()], [assert_reactive()], [assert_summary_funs()], [assert_adtte_vars()]
#'
#' @name assertions
#' @import checkmate
#' @keywords internal
#'
NULL

# assert_tag ----

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
check_tag <- function(x, null.ok = FALSE) { # nolint
  assert_flag(null.ok)
  ok <- (null.ok && test_null(x)) || test_class(x, "shiny.tag")
  if (!ok) {
    return("Must be a 'shiny.tag' or NULL")
  }
  return(TRUE)
}

#' @rdname check_tag
#' @inheritParams assertion_arguments
#' @export
assert_tag <- makeAssertionFunction(check_tag)

#' @rdname check_tag
#' @export
test_tag <- makeTestFunction(check_tag)

#' @rdname check_tag
#' @inheritParams assertion_arguments
#' @export
expect_tag <- makeExpectationFunction(check_tag)

# assert_reactive ----

#' Check for Reactive Input
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Check whether `x` is a reactive input.
#'
#' @inheritParams assertion_arguments
#' @seealso [`assertions`] for more details.
#'
#' @export
#'
#' @examples
#' check_reactive("bla")
#' check_reactive(reactive("bla"))
check_reactive <- function(x) {
  inherits(x, "reactive")
}

#' @rdname check_reactive
#' @inheritParams assertion_arguments
#' @export
assert_reactive <- makeAssertionFunction(check_reactive)

#' @rdname check_reactive
#' @export
test_reactive <- makeTestFunction(check_reactive)

# assert_summary_funs ----

#' Check for List of Summary Functions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Check whether `x` is a list of summary functions.
#'
#' @inheritParams assertion_arguments
#' @param null.ok (`flag`)\cr whether `x` may also contain `NULL`, meaning that
#'   a user choice is possible where no summary function should be applied.
#' @seealso [`assertions`] for more details.
#'
#' @export
#'
#' @examples
#' assert_summary_funs(list(mean = colMeans, raw = NULL), null.ok = TRUE)
assert_summary_funs <- function(x, null.ok = FALSE) { # nolint
  assert_flag(null.ok)
  assert_list(
    x,
    types = c("function", `if`(null.ok, "null", NULL)),
    min.len = 1L,
    unique = TRUE,
    names = "unique"
  )
  invisible(x)
}

# assert_adtte_vars ----

#' Check for `ADTTE` Variables
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Check whether `x` is a list of `ADTTE` variables.
#'
#' @inheritParams assertion_arguments
#' @seealso [`assertions`] for more details.
#'
#' @export
#'
#' @examples
#' assert_adtte_vars(list(aval = "AV", is_event = "EV", paramcd = "PC", usubjid = "ID", avalu = "u"))
assert_adtte_vars <- function(x) {
  assert_list(x, types = "character", names = "unique", unique = TRUE)
  assert_names(names(x), permutation.of = c("aval", "is_event", "paramcd", "usubjid", "avalu"))
  invisible(x)
}
