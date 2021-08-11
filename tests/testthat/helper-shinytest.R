# Add a convenience method to the R6 ShinyDriver class.
shinytest::ShinyDriver$set(
  which = "public",
  name = "getOutputValue",
  value = function(name) {
    assert_string(name, min.chars = 1L)
    self$waitForShiny()
    output_res <- self$getAllValues(
      output = name,
      input = FALSE,
      export = FALSE
    )[["output"]]
    if (name %in% names(output_res))
      output_res[[name]]
    else
      NULL
  },
  overwrite = TRUE
)
