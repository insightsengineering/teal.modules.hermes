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
  result <- expect_warning(expect_warning(
    h_km_mae_to_adtte(
      bad_adtte,
      mae,
      genes = hermes::gene_spec("GeneID:1820"),
      experiment_name = "hd2"
    ),
    "removed from MAE because not contained in ADTTE"
  ), # There are 2 warnings here.
  "'experiments' dropped; see 'metadata'"
  )
})

