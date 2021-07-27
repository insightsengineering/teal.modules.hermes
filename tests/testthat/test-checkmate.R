test_that("check_tag works as expected", {
  expect_identical(
    check_tag("bla"),
    "Must be a 'shiny.tag' or NULL"
  )
  expect_true(
    check_tag(NULL, null.ok = TRUE)
  )
  expect_true(
    check_tag(tags$h1("My text"))
  )
  expect_error(
    check_tag("bla", null.ok = "no")
  )
})
