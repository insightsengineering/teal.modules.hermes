# h_km_mae_to_adtte ----

test_that("h_km_mae_to_adtte function works as expected with a single gene", {
  mae <- hermes::multi_assay_experiment
  adtte <- teal.modules.hermes::rADTTE %>%
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
  adtte <- teal.modules.hermes::rADTTE %>%
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
  adtte <- teal.modules.hermes::rADTTE %>%
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
  adtte <- teal.modules.hermes::rADTTE %>%
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
  adtte <- teal.modules.hermes::rADTTE %>%
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
  adtte <- teal.modules.hermes::rADTTE %>%
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
        colData = data.frame(USUBJID = c("A", "B")) # USUBJID on experiment.
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
    "Must be a subset of {'C','D'}, but has additional elements {'A','B'}",
    fixed = TRUE
  )
})

test_that("h_km_mae_to_adtte fails as expected if USUBJID in experiment colData is different from sample map", {
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list(
      a = hermes::HermesDataFromMatrix(
        matrix(1:4, 2, 2, dimnames = list(c("ENSG1", "ENSG2"), c("E", "F"))),
        colData = data.frame(USUBJID = c("A", "B")) # USUBJID on experiment.
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
    "Must be a subset of {'E','F'}, but has additional elements {'A','B'}",
    fixed = TRUE
  )
})

# adtteSpecInput ----

test_that("adtteSpecInput creates expected HTML", {
  expect_silent(result <- adtteSpecInput(
    "adtte",
    label_paramcd = "Select right PARAMCD"
  ))

  expect_class(result, "shiny.tag.list")
  expect_length(result, 2)

  # First element is a div tag
  expect_tag(result[[1]])

  # Second element is the contents of a single js file
  expect_length(result[[2]], 1)
  expect_tag(result[[2]][[1]])
})

# nolint start

test_that("adtteSpecServer module works as expected in the test app", {
  skip_if_covr()
  skip_if_too_deep(5)

  app <- AppDriver$new(
    app_dir = test_path("adtteSpec"),
    name = "adtteSpecServe",
    variant = platform_variant(),
    load_timeout = 300000
  )

  app$wait_for_idle(timeout = 20000)
  ns <- module_ns_shiny2(app)

  # check initialization
  res <- app$get_values()
  expect_equal(res$input[[ns("experiment-name")]], "hd1")
  expect_equal(res$output[[ns("summary")]]$message, "please select at least one gene")

  # check correct message
  app$set_inputs(!!ns("genes-genes") := "GeneID:28")
  res <- app$get_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")

  app$set_inputs(!!ns("adtte-paramcd") := "CRSD")
  res <- app$get_value(output = ns("summary"))
  expect_match(res, "CRSD")

  app$set_inputs(!!ns("adtte-paramcd") := "PFS")
  res <- app$get_value(output = ns("summary"))
  expect_match(res, "PFS")

  res <- app$get_value(input = ns("adtte-paramcd"))

  # Test what happens if selected endpoint (here PFS) is no longer in filtered data.
  app$set_inputs(!!ns2("add-ADTTE-filter-var_to_add") := "PARAMCD")
  app$set_inputs(!!ns2("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection") := "OS")

  app$wait_for_idle(timeout = 20000)
  # We expect to get a validation message (also a notification box but we cannot test that)
  res <- app$get_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")
  res <- app$get_value(input = ns("adtte-paramcd"))
  expect_equal(res, "")

  # Now we update the filter by adding PFS back. However the user would have to
  # actively select it.
  app$set_inputs(!!ns2("active-ADTTE-filter-ADTTE_PARAMCD-inputs-selection") := c("PFS", "OS"))
  app$wait_for_idle()
  res <- app$get_value(output = ns("summary"))
  expect_equal(res$message, "please select an endpoint")

  app$stop()
})

# nolint end
