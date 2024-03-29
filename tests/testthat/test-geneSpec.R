# geneSpecInput ----

test_that("geneSpecInput creates expected HTML", {
  expect_silent(result <- geneSpecInput(
    "my_genes",
    funs = list(mean = colMeans),
    label_funs = "Please select function"
  ))
})

# h_parse_genes ----

test_that("h_parse_genes works as expected", {
  choices <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("a", "b", "c", "d", "e")
  )
  result <- h_parse_genes(c("2535", "a", "bla"), choices)
  expected <- choices[c(1, 3), ]
  expect_identical(result, expected)
})

test_that("h_parse_genes correctly returns empty data frame when no genes match", {
  choices <- data.frame(
    id = c("2533", "2534", "2535", "2536", "2537"),
    name = c("a", "b", "c", "d", "e")
  )
  result <- h_parse_genes(c("2539", "x", "bla"), choices)
  expected <- choices[NULL, ]
  expect_identical(result, expected)
})
