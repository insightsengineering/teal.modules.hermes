# nolint start

# assaySpecServer ----
test_that("assaySpecServer module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  ui <- function(id) {
    ns <- NS(id)

    teal.widgets::standard_layout(
      encoding = tags$div(
        uiOutput(ns("experiment_ui")),
        assaySpecInput(
          ns("assay"),
          label_assays = "Please choose assay"
        )
      ),
      output = verbatimTextOutput(ns("result"))
    )
  }

  server <- function(id,
                     data,
                     filter_panel_api,
                     mae_name) {
    moduleServer(id, function(input, output, session) {
      output$experiment_ui <- renderUI({
        experimentSpecInput(
          session$ns("experiment"),
          data = data,
          mae_name = "MAE"
        )
      })
      experiment <- experimentSpecServer(
        "experiment",
        data = data,
        filter_panel_api = filter_panel_api,
        mae_name = "MAE"
      )
      assay <- assaySpecServer(
        "assay",
        assays = experiment$assays,
        exclude_assays = c("counts", "cpm", "tpm", "bla")
      )
      output$result <- renderPrint({
        assay <- assay()
        assay
      })
    })
  }

  data <- teal.data::teal_data(MAE = hermes::multi_assay_experiment)

  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      teal::module(
        label = "assaySpec example",
        server = server,
        ui = ui,
        datanames = "all"
      )
    ),
    load_timeout = 300000
  )

  # Validation message because no assays eligible in first experiment.
  app$wait_for_idle(timeout = 20000)
  res <- app$get_active_module_output("result")
  expect_identical(res$message, "No assays eligible for this experiment, please make sure to add normalized assays")

  # Select the second experiment and see that we can select the right assays.
  app$set_module_input("experiment-name", "hd2")
  app$wait_for_idle()

  res <- app$get_active_module_input("assay-name")
  expect_identical(res, "rpkm")

  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"rpkm\"")

  app$set_module_input("assay-name", "voom")
  app$wait_for_idle()
  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"voom\"")

  # Check that cpm should not be available.
  app$set_module_input("assay-name", "cpm")
  app$wait_for_idle()
  res <- app$get_active_module_output("result")
  expect_identical(res, "[1] \"\"")

  app$stop()
})

# nolint end
