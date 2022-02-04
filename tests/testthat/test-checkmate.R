# check_tag ----

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

# assert_adtte_vars ----

test_that("assert_adtte_vars works as expected", {
  expect_silent(assert_adtte_vars(list(aval = "AV", is_event = "EV", paramcd = "PC", usubjid = "ID", avalu = "U")))
  expect_error(assert_adtte_vars(c(aval = "AV", is_event = "EV", paramcd = "PC", usubjid = "ID", avalu = "U")))
  expect_error(assert_adtte_vars(list(aval = "AV", paramcd = "PC")))
})
