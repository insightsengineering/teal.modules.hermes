# assaySpecInput ----

test_that("assaySpecInput creates expected HTML", {
  expect_snapshot(assaySpecInput(
    "my_assay",
    label_assays = "Please select the best assay"
  ))
})

# assaySpecServer ----

