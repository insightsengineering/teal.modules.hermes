test_that("is_blank works as expected", {
  expect_true(is_blank(""))
  expect_false(is_blank(" "))
  expect_false(is_blank(c("", "")))
})
