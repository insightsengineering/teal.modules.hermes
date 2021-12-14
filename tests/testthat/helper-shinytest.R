# Add convenience methods to the R6 ShinyDriver class.

# waitForOutputElement ----
shinytest::ShinyDriver$set(
  which = "public",
  name = "waitForOutputElement",
  value = function(name,
                   element,
                   ignore = list(NULL, ""),
                   timeout = 10000,
                   interval = 400) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_string(element, min.chars = 1L)
    checkmate::assert_list(ignore, types = c("null", "character"), min.len = 1L)
    checkmate::assert_number(timeout, lower = 1, finite = TRUE)
    checkmate::assert_number(interval, lower = 1, finite = TRUE)

    self$waitForShiny()

    timeout_sec <- timeout / 1000
    interval_sec <- interval / 1000
    time_num <- function() {
      as.numeric(Sys.time())
    }
    time_end <- time_num() + timeout_sec

    while (time_num() < time_end) {
      result <- try({
        output_res <- self$getAllValues(
          output = name,
          input = FALSE,
          export = FALSE
        )[["output"]]
        output_res[[name]][[element]]
      })

      if (!inherits(result, "try-error")) {
        invalid_match <- vapply(ignore, identical, logical(1L), x = result)
        # if no matches, then it's a success!
        if (!any(invalid_match)) {
          return(result)
        }
      }
      Sys.sleep(interval_sec)
    }
    stop(paste(
      "time out was reached while waiting for element",
      element, "from output", name
    ))
  },
  overwrite = TRUE
)

# setValues ----
shinytest::ShinyDriver$set(
  which = "public",
  name = "setValues",
  value = function(..., ns = NULL, iotype = c("auto", "input", "output")) {
    checkmate::assert_function(ns, args = "id", null.ok = TRUE)
    iotype <- match.arg(iotype)
    nm_val_pairs <- list(...)
    checkmate::assert_list(nm_val_pairs, names = "unique", min.len = 1L, null.ok = FALSE)

    for (nm in names(nm_val_pairs)) {
      val <- nm_val_pairs[[nm]]
      if (!is.null(ns)) {
        nm <- ns(nm)
      }
      self$setValue(
        name = nm,
        value = val,
        iotype = iotype
      )
    }
  },
  overwrite = TRUE
)

module_ns <- function(app) {
  source <- app$getSource()
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(source), css = ".teal_module"),
    "id"
  )
  ns <- NS(module_id)

}
