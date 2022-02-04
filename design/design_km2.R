library(dplyr)
library(random.cdisc.data)
mae <- hermes::multi_assay_experiment
adtte <- radtte(cached = TRUE) %>%
  mutate(CNSR = as.logical(CNSR)) %>%
  dplyr::filter(PARAMCD == "OS")
gene_var2 <- c("GeneID:1820", "GeneID:94115")
new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")
new_adtte2 <- h_km_mae_to_adtte(adtte, mae, gene_var = gene_var2, experiment_name = "hd2")

ANL <- new_adtte # nolint
names(ANL)
arm_var <- attr(ANL, "gene_cols")
mycode <- template_g_km_mae(arm_var = arm_var)
mycode
mapply(eval, mycode)
