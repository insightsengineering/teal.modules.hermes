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
    "All elements must have at least 1 characters, but element 1 has 0 characters."
  )
})

testthat::test_that("card_template function returns TealReportCard object with appropriate content and labels", {
  fd <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
  filter_panel_api <- teal.slice::FilterPanelAPI$new(fd)

  card <- shiny::isolate(card_template(title = "Card title",
                                       label =  "Card label",
                                       description =  NULL,
                                       filter_panel_api = filter_panel_api))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card label")
  testthat::expect_length(card$get_content(), 3)

  card <- shiny::isolate(card_template(title = "Card title",
                                       label =  "",
                                       description =  "Description text",
                                       filter_panel_api = filter_panel_api))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card title")
  testthat::expect_length(card$get_content(), 4)
})

