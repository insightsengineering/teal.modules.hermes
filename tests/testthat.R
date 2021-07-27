library(testthat)

test_results <- test_check("teal.modules.hermes")
saveRDS(test_results, "unit_testing_results.rds")
