# h_km_mae_to_adtte ----

test_that("h_km_mae_to_adtte works as expected with default settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))

  result <- h_km_mae_to_adtte(
    adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "hd2"
  )
  result2 <- h_km_mae_to_adtte(
    adtte,
    mae,
    gene_var = c("GeneID:1820", "GeneID:94115"),
    experiment_name = "hd2"
  )

  expect_class(result, "data.frame")
  expect_class(result2, "data.frame")
  expect_subset("SampleID", colnames(result))
  expect_true(any(grep("GeneID", colnames(result))))
})

test_that("h_km_mae_to_adtte fails as expected with invalid settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adtte %>%
    dplyr::mutate(CNSR = as.logical(.data$CNSR))
  good_adtte <- adtte

  # Example with no matched patient IDs.
  bad_adtte <- adtte %>% dplyr::mutate(USUBJID = paste0("bla-", USUBJID))

  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "hd4"
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "hd2",
    assay_name = "foo"
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = 1
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = 1520
  ))
  expect_error(h_km_mae_to_adtte(
    bad_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "hd2"
  ))
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
      gene_var = "ENSG1",
      experiment_name = "a"
    ),
    "Must be equal to set {'A','B'}, but is {'C','D'}",
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
      gene_var = "ENSG1",
      experiment_name = "a"
    ),
    "Must be equal to set {'E','F'}, but is {'A','B'}",
    fixed = TRUE
  )
})
