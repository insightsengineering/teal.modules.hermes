#' Data Preprocessing for KM Module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function to help with merging of MAE to ADTTE object for use with `g_km`.
#'
#' @note We require that each patient only has one sample.
#'
#' @inheritParams function_arguments
#'
#' @return A data frame containing all columns/rows from adtte and select columns from
#' MAE (assay, Sample IDs) for a given gene(s).
#'
#' @export
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' mae <- hermes::multi_assay_experiment
#' adtte <- radtte(cached = TRUE) %>%
#'   mutate(CNSR = as.logical(CNSR))
#'
#' # Make sure patient IDs match some in adtte to test the function.
#' experiment_name <- "se2"
#' se_test <- mae[[experiment_name]]
#' hd_test <- hermes::HermesData(se_test)
#' mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
#' samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
#' se_patients <- samplemap_experiment$primary
#' adtte$USUBJID[1:9] <- se_patients
#' gene_var <- c("GeneID:1820", "GeneID:94115")
#' new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "se2")
#' new_adtte2 <- h_km_mae_to_adtte(adtte, mae, gene_var = gene_var, experiment_name = "se2")
h_km_mae_to_adtte <- function (adtte,
                               mae,
                               gene_var,
                               experiment_name = "se1",
                               assay_name = "counts"){

  assert_choice(assay_name, c("counts", "cpm", "rpkm", "tpm", "voom"))
  assert_character(gene_var)
  assert_character(experiment_name)

  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  patients_in_experiment <- samplemap_experiment$primary

  assert_character(patients_in_experiment, unique = TRUE)

  merge_samplemap <- samplemap_experiment[, c("primary", "colname")]
  merge_samplemap <- as.data.frame(merge_samplemap)
  colnames(merge_samplemap) <- c("USUBJID", "SampleID")

  se <- mae[[experiment_name]]
  hd <- hermes::HermesData(se)

  num_genes <- length(gene_var)
  gene_assay <- SummarizedExperiment::assay(hd, assay_name)[gene_var,]
  gene_assay <- as.data.frame(gene_assay)

  if (num_genes == 1){
    colnames(gene_assay) <- paste(gene_var, assay_name, sep = "_")
    gene_assay$SampleID <- rownames(gene_assay)
  }

  if (num_genes > 1){
    rownames(gene_assay) <- paste(rownames(gene_assay), assay_name, sep = "_")
    gene_assay <- data.frame(t(gene_assay), SampleID = colnames(gene_assay))
  }

  merge_se_data <- merge(merge_samplemap, gene_assay, by = "SampleID", all.x = TRUE)

  adtte_patients <- unique(adtte$USUBJID)
  se_patients <- merge_se_data$USUBJID
  assert_false(any(!(se_patients %in% adtte_patients)))

  merged_adtte <- merge(adtte, merge_se_data, by = "USUBJID", all.x = TRUE)
  merged_adtte <- tern::df_explicit_na(merged_adtte)

  merged_adtte
}
