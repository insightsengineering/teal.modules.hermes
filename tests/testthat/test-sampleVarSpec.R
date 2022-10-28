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

# sampleVarSpecServer ----

test_that("sampleVarSpecServer only gives atomic columns with at least one value in col_data_vars", {
  object <- hermes::hermes_data
  new_col_data <- S4Vectors::DataFrame(
    ok = rep("a", ncol(object)),
    ok_too = c(NA, rep("b", ncol(object) - 1)),
    all_na = NA,
    row.names = colnames(object)
  )
  new_col_data$non_atomic_col <- S4Vectors::DataFrame(a = 1, b = 2)
  SummarizedExperiment::colData(object) <- new_col_data

  experiment_name <- reactiveVal("bla")
  original_data <- reactiveVal(object)
  testServer(
    sampleVarSpecServer,
    args = list(
      experiment_name = experiment_name,
      original_data = original_data
    ),
    expr = {
      col_data_vars <- col_data_vars()
      expect_set_equal(col_data_vars, c("ok", "ok_too"))
      expect_disjunct(col_data_vars, c("non_atomic_col", "all_na"))
    }
  )
})

test_that("sampleVarSpecServer only gives factor columns in col_data_vars when categorical_only", {
  object <- hermes::hermes_data
  SummarizedExperiment::colData(object) <- S4Vectors::DataFrame(
    char = rep("a", ncol(object)),
    num = 1,
    fac = factor("a"),
    row.names = colnames(object)
  )

  experiment_name <- reactiveVal("bla")
  original_data <- reactiveVal(object)
  testServer(
    sampleVarSpecServer,
    args = list(
      experiment_name = experiment_name,
      original_data = original_data,
      categorical_only = TRUE
    ),
    expr = {
      col_data_vars <- col_data_vars()
      expect_identical(col_data_vars, "fac")
      expect_disjunct(col_data_vars, c("char", "num"))
    }
  )
})

# nolint start

test_that("sampleVarSpec module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = "sampleVarSpec",
    name = "sampleVarSpec module works as expected in the test app",
    variant = platform_variant()
  )

  app$wait_for_idle()
  ns <- module_ns_shiny2(app)

  # Initially no variable is selected.
  res <- app$get_value(input = ns("facet_var"))
  expect_null(res)

  # Select a variable.
  first_var <- "AGE18"
  app$set_inputs(!!ns("facet_var-sample_var") := first_var)

  # Check the output and which levels are reported there.
  res <- app$wait_for_value(output = ns("summary"))
  expect_match(res, " < 18 >= 18 \n")

  # Now click on the levels button, set combination and click ok.
  app$click(ns("facet_var-levels_button"))
  first_combination <- list("< 18" = "2", ">= 18" = "2")
  # Click on second column in both rows.
  app$wait_for_idle()
  app$set_inputs(!!ns("facet_var-comb_assignment") := first_combination)
  app$wait_for_idle()
  app$click(ns("facet_var-ok"))

  # Check the output and which levels are reported there.
  res <- app$wait_for_value(output = ns("summary"))
  expect_match(res, "< 18/>= 18 \n")
})

# nolint end
