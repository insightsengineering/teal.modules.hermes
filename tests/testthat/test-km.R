# h_km_mae_to_adtte ----
library(random.cdisc.data)
library(data.table)

test_that("h_km_mae_to_adtte function works as expected with default settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>% dplyr::mutate(CNSR = as.logical(CNSR))
  gene_var2 <- c("GeneID:1820", "GeneID:94115")
  result <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "se2")
  result2 <- h_km_mae_to_adtte(adtte, mae, gene_var = gene_var2, experiment_name = "se2")

  expect_class(result, "data.frame")
  expect_class(result2, "data.frame")
  expect_true("SampleID" %in% colnames(result))
  expect_true(any(colnames(result) %like% "GeneID"))
})

test_that("h_km_mae_to_adtte fails as expected with invalid settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>% dplyr::mutate(CNSR = as.logical(CNSR))
  # Example with no matched patient IDs.
  bad_adtte <- adtte

  # Make sure patient IDs match some in adtte to test function.
  experiment_name <- "se2"
  se_test <- mae[[experiment_name]]
  hd_test <- hermes::HermesData(se_test)
  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  se_patients <- samplemap_experiment$primary
  adtte$USUBJID[1:9] <- se_patients
  good_adtte <- adtte

  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "se4"
  ))
  expect_error(h_km_mae_to_adtte(
    good_adtte,
    mae,
    gene_var = "GeneID:1820",
    experiment_name = "se2",
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
    gene_var = "GeneID:1820"
  ))
})
