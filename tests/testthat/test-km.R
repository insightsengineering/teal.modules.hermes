# h_km_mae_to_adtte ----

test_that("h_km_mae_to_adtte function works as expected with a single gene", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  result <- h_km_mae_to_adtte(
    adtte,
    mae,
    genes = hermes::gene_spec("GeneID:1820"),
    experiment_name = "hd2"
  )
  expect_class(result, "data.frame")
  expect_subset(c("SampleID", "GeneID1820_counts"), colnames(result))
})

test_that("h_km_mae_to_adtte function works as expected with multiple genes", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  result <- h_km_mae_to_adtte(
    adtte,
    mae,
    genes = hermes::gene_spec(c("GeneID:1820", "GeneID:94115")),
    experiment_name = "hd2"
  )
  expect_class(result, "data.frame")
  expect_subset(c("SampleID", "GeneID1820_counts", "GeneID94115_counts"), colnames(result))
})

test_that("h_km_mae_to_adtte function works as expected with a gene signature", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  result <- h_km_mae_to_adtte(
    adtte,
    mae,
    genes = hermes::gene_spec(c("GeneID:1820", "GeneID:94115"), fun = colMeans),
    experiment_name = "hd2",
    assay_name = "cpm"
  )
  expect_class(result, "data.frame")
  expect_subset(c("SampleID", "colMeansGeneID1820GeneID94115_cpm"), colnames(result))
})

test_that("h_km_mae_to_adtte fails as expected with invalid settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))
  good_adtte <- adtte

  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    genes = hermes::gene_spec("GeneID:1820"),
    experiment_name = "hd4"
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    genes = hermes::gene_spec("GeneID:1820"),
    experiment_name = "hd2",
    assay_name = "foo"
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    genes = hermes::gene_spec("GeneID:1820"),
    experiment_name = 1
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    genes = 1520
  ))
})

test_that("h_km_mae_to_adtte warns when patients are not in ADTTE and therefore removed", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  # Example with no matched patient IDs.
  bad_adtte <- adtte %>% dplyr::mutate(USUBJID = paste0("bla-", USUBJID))
  result <- expect_warning(
    h_km_mae_to_adtte(
      bad_adtte,
      mae,
      genes = hermes::gene_spec("GeneID:1820"),
      experiment_name = "hd2"
    ),
    "removed from MAE because not contained in ADTTE"
  )
})

test_that("h_km_mae_to_adtte fails as expected if USUBJID in MAE colData is different from sample map", {
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list(
      a = hermes::HermesDataFromMatrix(
        matrix(1:4, 2, 2, dimnames = list(c("ENSG1", "ENSG2"), c("A", "B"))),
        colData = data.frame(USUBJID = c("A", "B"))  # USUBJID on experiment.
      )
    ),
    colData = data.frame(
      USUBJID = c("C", "D"), # USUBJID on MAE.
      row.names = c("A", "B")
    )
  )
  adtte <- data.frame(USUBJID = c("A", "B"))

  expect_error(
    h_km_mae_to_adtte(
      adtte,
      mae,
      genes = hermes::gene_spec("ENSG1"),
      experiment_name = "a"
    ),
    "Must be a subset of {'C','D'}, but is {'A','B'}",
    fixed = TRUE
  )
})

test_that("h_km_mae_to_adtte fails as expected if USUBJID in experiment colData is different from sample map", {
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list(
      a = hermes::HermesDataFromMatrix(
        matrix(1:4, 2, 2, dimnames = list(c("ENSG1", "ENSG2"), c("E", "F"))),
        colData = data.frame(USUBJID = c("A", "B"))  # USUBJID on experiment.
      )
    ),
    colData = data.frame(
      USUBJID = c("A", "B"),
      row.names = c("E", "F") # USUBJID on sample map.
    )
  )
  adtte <- data.frame(USUBJID = c("E", "F"))

  expect_error(
    h_km_mae_to_adtte(
      adtte,
      mae,
      genes = hermes::gene_spec("ENSG1"),
      experiment_name = "a"
    ),
    "Must be a subset of {'E','F'}, but is {'A','B'}",
    fixed = TRUE
  )
})

# ui_g_km ----

test_that("ui_g_km creates expected HTML", {
  mae_name <- "MyMAE"
  set.seed(123)
  datasets <- mock_datasets(list(MyMAE = hermes::multi_assay_experiment))
  expect_snapshot(ui_g_km(
    id = "testid",
    datasets = datasets,
    mae_name = mae_name,
    summary_funs = list(
      Mean = colMeans
    ),
    pre_output = NULL,
    post_output = NULL
  ))
})

# tm_g_km ----

test_that("tm_g_km works as expected in the sample app", {
  test.nest::skip_if_too_deep(5)

  skip_if_covr()

  library(shinytest)
  app <- ShinyDriver$new("km/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  ns <- NS("teal-main_ui-modules_ui-root_kaplan_meier")

  # Check initial state of encodings.
  initial_experiment_name <- app$waitForValue(ns("experiment-name"))
  expect_identical(initial_experiment_name, "hd1")

  plot_message <- app$waitForOutputElement(ns("km_plot"), "message")
  expect_identical(
    plot_message,
    "No assays eligible for this experiment, please make sure to add normalized assays"
  )

  # Choose another experiment.
  app$setValue(ns("experiment-name"), "hd2")

  initial_assay_name <- app$waitForValue(ns("assay-name"))
  expect_identical(initial_assay_name, "cpm")

  # Choose a gene signature.
  app$setValue(ns("genes-genes"), c("GeneID:101927746", "GeneID:1820"))

  # Initial plot.
  expect_snapshot_screenshot(
    app,
    id = ns("km_plot"),
    name = "initial_plot.png"
  )

  app$stop()
})
