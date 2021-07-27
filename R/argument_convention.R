# module_arguments ----

#' Standard Module Arguments
#'
#' The documentation to this function lists all the conventional arguments in
#' `hermes` teal modules.
#'
#' @param datasets (`Datasets`)\cr
#'   teal specific argument which is automatically passed to the UI and server
#'   functions, holding all the data sets provided in the app initialization.
#' @param label (`string`)\cr
#'   menu item label of the module in the teal app.
#' @param id (`string`)\cr
#'   the ID used to call the teal module's UI function.
#' @param mae_name (`string`)\cr
#'   name of the MAE data used in the teal module.
#' @param pre_output (`shiny.tag` or `NULL`)\cr
#'   placed before the output to put the output into context (for example a title).
#' @param post_output (`shiny.tag` or `NULL`)\cr
#'   placed after the output to put the output into context (for example the [shiny::helpText()]
#'   elements can be useful).
#' @param input (`ReactiveValues`)\cr the session's input object.
#' @param output (`shinyoutput`)\cr the session's output object.
#' @param session (`ShinySession`)\cr the session object.
#'
#' @name module_arguments
NULL

# assertion_arguments ----

#' Standard Assertion Arguments
#'
#' The documentation to this function lists all the conventional arguments in
#' additional `checkmate` assertions.
#'
#' @param x an object to check.
#' @param null.ok (`flag`)\cr whether `x` may also be `NULL`.
#' @param .var.name	(`string`)\cr name of the checked object to print in
#'   assertions; defaults to the heuristic implemented in [checkmate::vname()].
#' @param add (`AssertCollection` or `NULL`)\cr collection to store
#'   assertion messages, see [`checkmate::AssertCollection`].
#'
#' @name assertion_arguments
NULL
