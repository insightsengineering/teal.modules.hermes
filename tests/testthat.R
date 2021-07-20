if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)

  test_results <- test_check("teal.modules.hermes", wrap = FALSE)
  saveRDS(test_results, "unit_testing_results.rds")
}
