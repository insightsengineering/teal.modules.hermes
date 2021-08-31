test_that("is_blank works as expected", {
  expect_true(is_blank(""))
  expect_false(is_blank(" "))
  expect_false(is_blank(c("", "")))
})

test_that("h_extract_words works as expected", {
  expect_identical(
    h_extract_words("a, b, , c, 234; 34562 - GeneID:bla"),
    c("a", "b", "c", "234", "34562", "GeneID:bla")
  )
  expect_identical(
    h_extract_words("GeneID:1820, sdf.393; 32596"),
    c("GeneID:1820", "sdf.393", "32596")
  )
  expect_error(
    h_extract_words(""),
    "Must have at least 1 characters"
  )
})
