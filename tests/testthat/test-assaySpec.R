# assaySpecInput ----

test_that("assaySpecInput creates expected HTML", {
  expect_silent(result <- assaySpecInput(
    "my_assay",
    label_assays = "select assay"
  ))

  expect_class(result, "shiny.tag.list")
  expect_length(result, 2)

  # First element is a div tag
  expect_tag(result[[1]])

  # Second element is the contents of a single js file
  expect_length(result[[2]], 1)
  expect_tag(result[[2]][[1]])
})
