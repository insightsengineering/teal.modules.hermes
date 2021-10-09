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
#' @param inputId (`string`)\cr
#'   the ID used to call the module input.
#' @param adtte_name (`string`)\cr
#'   name of the ADTTE dataset.
#' @param adtte_vars (named `list` of `string`)\cr
#'   names of the variables to use in the ADTTE dataset. It should comprise elements:
#'   - `aval`: the numeric time-to-event variable.
#'   - `avalu`: the variable holding the unit of `aval`.
#'   - `is_event`: the logical event variable. It needs to be `TRUE`
#'        when there was an observed event, and `FALSE` if the time is censored without
#'        observed event.
#'   - `paramcd`: the character or factor parameter code variable, defining the
#'       type of time-to-event for selection in the module.
#'   - `usubjid`: the subject ID variable.
#' @param mae_name (`string`)\cr
#'   name of the MAE data used in the teal module.
#' @param exclude_assays (`character`)\cr
#'   names of the assays which should not be included in choices in the teal module.
#' @param summary_funs (named `list` of functions or `NULL`)\cr functions which can be used
#'   in the the gene signatures. For modules that support also multiple genes without
#'   summary, `NULL` can be included to not summarize the genes but provide all of them.
#' @param pre_output (`shiny.tag` or `NULL`)\cr
#'   placed before the output to put the output into context (for example a title).
#' @param post_output (`shiny.tag` or `NULL`)\cr
#'   placed after the output to put the output into context (for example the [shiny::helpText()]
#'   elements can be useful).
#' @param input (`ReactiveValues`)\cr the session's input object.
#' @param output (`shinyoutput`)\cr the session's output object.
#' @param session (`ShinySession`)\cr the session object.
#' @param plot_height (`list`)\cr list of integers to set the default, minimum,
#'   and maximum plot height.
#' @param plot_width (`list`)\cr list of integers to set the default, minimum,
#'   and maximum plot width.
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
#' @param info (`string`)\cr extra information to be included in the
#'   message for the testthat reporter, see [testthat::expect_that()].
#' @param label (`string`)\cr name of the checked object to print in
#'   messages. Defaults to the heuristic implemented in [checkmate::vname()].
#'
#' @name assertion_arguments
NULL

# function_arguments ----

#' Standard Function Arguments
#'
#' The documentation to this function lists all the conventional arguments in
#' functions.
#'
#' @param adtte (`data frame`)\cr an `adtte` dataset.
#' @param usubjid_var (`string`)\cr variable name of the subject ID variable.
#' @param mae (`MultiAssayExperiment`)\cr contains `AnyHermesData` objects.
#' @param object (`AnyHermesData`)\cr contains RNA-seq values for one experiment.
#' @param genes (`GeneSpec`)\cr specification for gene(s) (signature), e.g.
#'   using [hermes::gene_spec()].
#' @param experiment_name (`string`)\cr the desired `HermesData` to use.
#' @param assay_name (`string`)\cr the assay to define the groups.
#'
#' @name function_arguments
NULL
