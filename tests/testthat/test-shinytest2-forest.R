# ui_g_forest_tte ----

test_that("e2e: tm_g_forest_tte initializes without errors and snapshot test", {
  data <- within(teal_data(), {
    ADTTE <- teal.modules.hermes::rADTTE %>% # nolint
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment # nolint
  })
  datanames <- c("ADTTE", "MAE")
  datanames(data) <- datanames
  join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_forest_tte(
        label = "forest",
        adtte_name = "ADTTE",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_no_shiny_error()

  app$expect_screenshot(name = "app")
  app$stop()
})

# tm_g_forest_tte ----

# nolint start

test_that("e2e: tm_g_forest_tte module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  data <- within(teal_data(), {
    ADTTE <- teal.modules.hermes::rADTTE %>% # nolint
      dplyr::mutate(is_event = .data$CNSR == 0)
    MAE <- hermes::multi_assay_experiment # nolint
  })
  datanames <- c("ADTTE", "MAE")
  datanames(data) <- datanames
  join_keys(data)["ADTTE", "ADTTE"] <- c("STUDYID", "USUBJID", "PARAMCD")

  app <- teal:::TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_g_forest_tte(
        label = "forest",
        adtte_name = "ADTTE",
        mae_name = "MAE",
        .test = TRUE
      )
    ),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)


  # check initialization
  res <- app$get_active_module_input("experiment-name")
  expect_identical(res, "hd1")

  res <- app$get_active_module_output("table")
  expect_identical(
    res$message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$set_module_input("experiment-name", "hd2")

  res <- app$wait_for_active_module_value(input = "assay-name")
  expect_identical(res, "cpm")

  # Choose a gene signature.
  app$set_module_input("genes-genes", c("GeneID:101927746", "GeneID:1820"))

  res <- app$wait_for_active_module_value(output = "plot-plot_out_main")
  expect_identical(res$message, "please select an endpoint")

  # Choose an endpoint.
  app$set_module_input("adtte-paramcd", "PFS")

  app$wait_for_idle()
  app$expect_screenshot(selector = app$active_module_element("table"))


  app$stop()
})

# nolint end
