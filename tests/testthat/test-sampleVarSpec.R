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
    c(
      "MULTIPLE", "MULTIPLE", "BLACK OR AFRICAN AMERICAN", "BLACK OR AFRICAN AMERICAN",
      "MULTIPLE", "WHITE", "UNKNOWN", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "MULTIPLE"
    )
  )
  group_list <- list(
    "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
    "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
    "WHITE" = "WHITE"
  )
  result <- h_collapse_levels(x, group_list)
  expected <- factor(
    c(
      "MULTIPLE/UNKNOWN", "MULTIPLE/UNKNOWN", "ASIAN/BLACK OR AFRICAN AMERICAN",
      "ASIAN/BLACK OR AFRICAN AMERICAN", "MULTIPLE/UNKNOWN", "WHITE",
      "MULTIPLE/UNKNOWN", "ASIAN/BLACK OR AFRICAN AMERICAN", "ASIAN/BLACK OR AFRICAN AMERICAN",
      "MULTIPLE/UNKNOWN"
    ),
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
