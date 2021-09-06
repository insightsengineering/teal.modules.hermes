# experimentSpecInput ----

test_that("experimentSpecInput creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(experimentSpecInput(
    "my_experiment",
    datasets = datasets,
    mae_name = mae_name,
    label_experiments = "Please select the best experiment"
  ))
})
