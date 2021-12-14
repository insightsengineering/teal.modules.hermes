# sampleVarSpecInput ----

test_that("sampleVarSpecInput creates expected HTML", {
  expect_snapshot(sampleVarSpecInput(
    "my_sample_var",
    label_vars = "Select cool variable",
    label_levels_button = "Combine those levels"
  ))
})

# h_assign_to_group_list ----

test_that("h_assign_to_group_list works as expected", {
  assign_list <- list(
    "ASIAN" = "1",
    "BLACK OR AFRICAN AMERICAN" = "1",
    "MULTIPLE" = "2",
    "UNKNOWN" = "2",
    "WHITE" = "4"
  )
  result <- h_assign_to_group_list(assign_list)
  expected <- list(
    "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
    "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
    "WHITE" = "WHITE"
  )
  expect_identical(result, expected)
})

test_that("h_assign_to_group_list retains the order of the collapsed levels", {
  assign_list <- list(
    "ASIAN" = "2",
    "BLACK OR AFRICAN AMERICAN" = "2",
    "MULTIPLE" = "1",
    "UNKNOWN" = "1",
    "WHITE" = "4"
  )
  result <- h_assign_to_group_list(assign_list)
  expected <- list(
    "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
    "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
    "WHITE" = "WHITE"
  )
  expect_identical(result, expected)
})

# h_collapse_levels ----

test_that("h_collapse_levels works as expected when a group_list is provided", {
  x <- factor(
    c("MULTIPLE", "MULTIPLE", "BLACK OR AFRICAN AMERICAN", "BLACK OR AFRICAN AMERICAN",
      "MULTIPLE", "WHITE", "UNKNOWN", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "MULTIPLE")
  )
  group_list <- list(
    "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
    "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
    "WHITE" = "WHITE"
  )
  result <- h_collapse_levels(x, group_list)
  expected <- factor(
    c("MULTIPLE/UNKNOWN", "MULTIPLE/UNKNOWN", "ASIAN/BLACK OR AFRICAN AMERICAN",
      "ASIAN/BLACK OR AFRICAN AMERICAN", "MULTIPLE/UNKNOWN", "WHITE",
      "MULTIPLE/UNKNOWN", "ASIAN/BLACK OR AFRICAN AMERICAN", "ASIAN/BLACK OR AFRICAN AMERICAN",
      "MULTIPLE/UNKNOWN"),
    levels = names(group_list)
  )
  expect_identical(result, expected)
})

test_that("h_collapse_levels fails when no group_list is provided", {
  x <- factor(1:10)
  expect_error(h_collapse_levels(x, group_list = NULL))
})

test_that("h_collapse_levels fails when x is not a factor provided", {
  x <- 1:10
  expect_error(h_collapse_levels(x, group_list = list(a = "b")))
})

# validate_n_levels ----

test_that("validate_n_levels passes for a valid factor with required number of levels", {
  x <- factor(1:3)
  result <- expect_silent(validate_n_levels(x, name = "z", n_levels = 3L))
  expect_null(result)
})

test_that("validate_n_levels gives expected validation error when x is not a factor", {
  x <- 1:3
  expect_error(
    validate_n_levels(x, name = "z", n_levels = 3L),
    "Variable z is not a factor but a integer"
  )
})

test_that(paste(
  "validate_n_levels gives expected validation error when",
  "factor x does not have required number of levels"
), {
  x <- factor(1:3)
  expect_error(
    validate_n_levels(x, name = "y", n_levels = 4L),
    "Please combine the original levels of y into exactly 4 levels"
  )
})

# sampleVarSpecServer ----

test_that("sampleVarSpec module works as expected in the test app", {
  skip_if_covr()
  utils.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new(testthat::test_path("sampleVarSpec"), loadTimeout = 1e5,
                         debug = "all", phantomTimeout = 1e5, seed = 123)
  on.exit(app$stop())
  app$getDebugLog()
  app$snapshotInit("test-app")
  Sys.sleep(2.5)
  module_id <- rvest::html_attr(
    rvest::html_node(rvest::read_html(app$getSource()), css = ".teal_module"),
    "id"
  )
  ns <- NS(module_id)

  # Initially no variable is selected.
  initial_var <- app$waitForValue(ns("facet_var"), ignore = "")
  expect_identical(initial_var, NULL)

  # Select a variable.
  first_var <- "AGE18"
  app$setValue(ns("facet_var-sample_var"), first_var)

  # Check the output and which levels are reported there.
  first_output <- app$waitForValue(ns("summary"), iotype = "output")
  expect_match(first_output, " < 18 >= 18 \n")

  # Now click on the levels button, set combination and click ok.
  app$click(ns("facet_var-levels_button"))
  first_combination <- list("< 18" = "2", ">= 18" = "2")  # Click on second column in both rows.
  app$waitForValue(ns("facet_var-comb_assignment"))
  app$setValue(ns("facet_var-comb_assignment"), first_combination)
  app$waitForValue(ns("facet_var-comb_assignment"))
  app$click(ns("facet_var-ok"))

  # Check the output and which levels are reported there.
  second_output <- app$waitForValue(ns("summary"), iotype = "output")
  expect_match(second_output, "< 18/>= 18 \n")
})
