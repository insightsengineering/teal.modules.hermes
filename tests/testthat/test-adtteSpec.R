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
  expect_subset("GeneID.1820", colnames(result))
})

test_that("h_km_mae_to_adtte function also works when some ID variables are factors", {
  mae <- hermes::multi_assay_experiment
  SummarizedExperiment::colData(mae)$USUBJID <- # nolint
    factor(SummarizedExperiment::colData(mae)$USUBJID)
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(USUBJID = factor(USUBJID))

  result <- h_km_mae_to_adtte(
    adtte,
    mae,
    genes = hermes::gene_spec("GeneID:1820"),
    experiment_name = "hd2"
  )
  expect_class(result, "data.frame")
  expect_subset("GeneID.1820", colnames(result))
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
  expect_subset(c("GeneID.1820", "GeneID.94115"), colnames(result))
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
  expect_subset("colMeans.GeneID.1820..GeneID.94115.", colnames(result))
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
    "from gene data set were lost"
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

# adtteSpecInput ----

test_that("adtteSpecInput creates expected HTML", {
  expect_snapshot(adtteSpecInput(
    "adtte",
    label_paramcd = "Select right PARAMCD"
  ))
})

# adtteSpecServer ----

test_that("adtteSpecServer module works as expected in the test app", {
  skip_if_covr()
  utils.nest::skip_if_too_deep(5)

  library(shinytest)
  app <- ShinyDriver$new("adtteSpec/", loadTimeout = 1e5, debug = "all", phantomTimeout = 1e5)
  app$getDebugLog()
  app$snapshotInit("test-app")

  ns <- NS("teal-main_ui-modules_ui-root_adtteSpec_example")

  msg <- app$waitForOutputElement(ns("summary"), "message")
  app$setValue(ns("genes-genes"), "GeneID:101927746")

  # Upon initialization the endpoint is not selected automatically, the user
  # has to click this actively.
  msg <- app$waitForOutputElement(ns("summary"), "message", ignore = list(msg))
  expect_identical(msg, "please select an endpoint")
  app$setValue(ns("adtte-paramcd"), "CRSD")

  summary_result <- app$waitForValue(ns("summary"), iotype = "output")
  expect_match(summary_result, "CRSD:5")

  app$setValue(ns("adtte-paramcd"), "PFS")
  summary_result <- app$waitForValue(ns("summary"), iotype = "output")
  expect_match(summary_result, "PFS:5")

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  ns2 <- NS("teal-main_ui-filter_panel")
  app$setValue(ns2("add_ADTTE_filter-filter-var_to_add"), "PARAMCD")
  app$waitForValue(ns2("add_ADTTE_filter-filter-var_PARAMCD-content-selection"))
  app$setValue(ns2("add_ADTTE_filter-filter-var_PARAMCD-content-selection"), "OS")

  # We expect to get a validation message (also a notification box but we cannot test that).
  msg <- app$waitForOutputElement(ns("summary"), "message")
  expect_identical(msg, "please select an endpoint")
  paramcd <- app$waitForValue(ns("adtte-paramcd"), ignore = list(NULL))
  expect_identical(paramcd, "")

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$setValue(ns2("add_ADTTE_filter-filter-var_PARAMCD-content-selection"), c("PFS", "OS"))
  msg <- app$waitForOutputElement(ns("summary"), "message")
  expect_identical(msg, "please select an endpoint")

  app$stop()
})

test_that("results from adtteSpecServer are as expected", {
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(is_event = .data$CNSR == 0)
  datasets <- mock_datasets(list(
    MyMAE = hermes::multi_assay_experiment,
    MyADTTE = adtte
  ))
  experiment_data <- reactiveVal(hermes::multi_assay_experiment[[1]])
  experiment_name <- reactiveVal(names(hermes::multi_assay_experiment)[1])
  assay <- reactiveVal("counts")
  genes <- reactiveVal(hermes::gene_spec("GeneID:101927746"))
  probs <- reactiveVal(0.33)
  testServer(
    adtteSpecServer,
    args = list(
      datasets = datasets,
      mae_name = "MyMAE",
      adtte_name = "MyADTTE",
      adtte_vars = list(
        aval = "AVAL",
        avalu = "AVALU",
        is_event = "is_event",
        paramcd = "PARAMCD",
        usubjid = "USUBJID"
      ),
      experiment_data = experiment_data,
      experiment_name = experiment_name,
      assay = assay,
      genes = genes,
      probs = probs
    ),
    expr = {
      session$setInputs(paramcd = "CRSD")

      time_unit <- time_unit()
      expect_identical(time_unit, "DAYS")

      gene_col <- gene_col()
      expect_identical(gene_col, "GeneID.101927746")

      dat <- binned_adtte_subset()
      expect_data_frame(dat)
      expect_identical(unique(as.character(dat$PARAMCD)), "CRSD")
      expect_factor(dat$gene_factor)
      expect_numeric(dat[[gene_col]])
    }
  )
})
