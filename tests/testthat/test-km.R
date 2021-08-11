# h_km_mae_to_adtte ----
library(random.cdisc.data)
library(data.table)

test_that("h_km_mae_to_adtte function works as expected with default settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>% dplyr::mutate(CNSR = as.logical(CNSR))
  gene_var2 <- c("GeneID:1820", "GeneID:94115")
  result <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")
  result2 <- h_km_mae_to_adtte(adtte, mae, gene_var = gene_var2, experiment_name = "hd2")

  expect_class(result, "data.frame")
  expect_class(result2, "data.frame")
  expect_true("SampleID" %in% colnames(result))
  expect_true(any(colnames(result) %like% "GeneID"))
})

test_that("h_km_mae_to_adtte fails as expected with invalid settings", {
  mae <- hermes::multi_assay_experiment
  adtte <- radtte(cached = TRUE) %>% dplyr::mutate(CNSR = as.logical(CNSR))
  good_adtte <- adtte

  # Example with no matched patient IDs.
  fake_patients <- rep("Pheobe", 9)
  adtte$USUBJID[1:9] <- fake_patients
  bad_adtte <- adtte

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
    gene_var = "GeneID:1820"
  ))
})
