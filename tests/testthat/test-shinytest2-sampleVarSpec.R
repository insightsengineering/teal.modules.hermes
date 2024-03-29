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

  ui <- function(id) {
    ns <- NS(id)

    teal.widgets::standard_layout(
      encoding = tags$div(
        uiOutput(ns("experiment_ui")),
        sampleVarSpecInput(ns("facet_var"), "Select variable")
      ),
      output = verbatimTextOutput(ns("summary"))
    )
  }

  server <- function(id,
                     data,
                     filter_panel_api) {
    moduleServer(id, function(input, output, session) {
      output$experiment_ui <- renderUI({
        experimentSpecInput(session$ns("experiment"), data, "MAE")
      })
      experiment <- experimentSpecServer(
        "experiment",
        data,
        filter_panel_api,
        "MAE"
      )
      facet_var_spec <- sampleVarSpecServer(
        "facet_var",
        experiment_name = experiment$name,
        original_data = experiment$data
      )
      output$summary <- renderPrint({
        experiment_data_final <- facet_var_spec$experiment_data()
        facet_var <- facet_var_spec$sample_var()
        req(facet_var)
        facet_col <- SummarizedExperiment::colData(experiment_data_final)[[facet_var]]
        summary(facet_col)
      })
    })
  }

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)
  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "sampleVarSpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    )
  )

  app$wait_for_idle(timeout = 20000)


  # Initially no variable is selected.
  res <- app$get_active_module_input("facet_var-sample_var")
  expect_null(res)

  # Select a variable.
  app$set_module_input("facet_var-sample_var", "AGE18")
  app$wait_for_idle()

  # Check the output and which levels are reported there.
  res <- app$get_active_module_output("summary")
  expect_match(as.character(res), " < 18 >= 18 \n    4     1 ")

  # Filter panel works as expected
  app$set_inputs("teal-main_ui-filter_panel-add-MAE-subjects-var_to_add" = "AGE18")
  app$wait_for_idle()
  app$set_inputs("teal-main_ui-filter_panel-active-MAE-subjects-MAE_AGE18-inputs-selection" = "< 18")
  app$wait_for_idle()

  res <- app$get_active_module_output("summary")
  expect_equal(res, "< 18 \n   4 ")

  app$click("teal-main_ui-filter_panel-active-MAE-subjects-MAE_AGE18-remove")
  res <- app$get_active_module_output("summary")
  expect_equal(res, " < 18 >= 18 \n    4     1 ")

  # Now click on the levels button, set combination and click ok.
  app$click(ns("facet_var-levels_button"))

  # Click on second column in both rows.
  app$wait_for_idle()
  app$set_module_input("facet_var-comb_assignment", list("< 18" = "2", ">= 18" = "2"))
  app$wait_for_idle()
  app$click(ns("facet_var-ok"))
  app$wait_for_idle()

  # Check the output and which levels are reported there.
  res <- app$get_active_module_output("summary")
  expect_match(res, "< 18/>= 18 \n         5 ")
  app$stop()
})

# nolint end
