library(dplyr)
library(random.cdisc.data)
mae <- hermes::multi_assay_experiment
adtte <- radtte(cached = TRUE) %>%
  mutate(CNSR = as.logical(CNSR))
gene_var2 <- c("GeneID:1820", "GeneID:94115")
new_adtte <- h_km_mae_to_adtte(adtte, mae, gene_var = "GeneID:1820", experiment_name = "hd2")
new_adtte2 <- h_km_mae_to_adtte(adtte, mae, gene_var = gene_var2, experiment_name = "hd2")

# We receive an error I believe related to the colon in GeneIDs. Just to check if we still
# get the error with a different col name.
new_adtte$renametest <- new_adtte$`GeneID:1820counts`
ANL <- new_adtte

mycode <- template_g_km(arm_var = "renametest")

mycode

# $preprocessing
{
  anl <- ANL %>% dplyr::mutate(renametest = tern::cut_quantile_bins(renametest,
                                                                    probs = c(0.33, 0.66)))
}

# $data
{
  anl <- anl %>% mutate(renametest = droplevels(renametest)) %>%
    dplyr::mutate(is_event = CNSR == 0)
}

# $variables
variables <- list(tte = "AVAL", is_event = "is_event", arm = "renametest")

# $graph
{
  grid::grid.newpage()
  lyt <- grid::grid.layout(nrow = nlevels(ANL$SEX), ncol = 1) %>%
    grid::viewport(layout = .) %>% grid::pushViewport()
  result <- mapply(df = split(anl, f = anl$SEX), nrow = seq_along(levels(anl$SEX)),
                   FUN = function(df_i, nrow_i) {
                     if (nrow(df_i) == 0) {
                       grid::grid.text("No data found for a given facet value.",
                                       x = 0.5, y = 0.5, vp = grid::viewport(layout.pos.row = nrow_i,
                                                                             layout.pos.col = 1))
                     }
                     else {
                       tern::g_km(df = df_i, variables = variables,
                                  font_size = 8, xlab = paste0("Survival time",
                                                               " (", gsub("(^|[[:space:]])([[:alpha:]])",
                                                                          "\\1\\U\\2", tolower(anl$AVALU[1]), perl = TRUE),
                                                               ")"), yval = "Survival", xticks = NULL, newpage = FALSE,
                                  title = paste("KM Plot", ",", quote(SEX), "=",
                                                as.character(unique(df_i$SEX))), ggtheme = theme_minimal(),
                                  annot_surv_med = TRUE, annot_coxph = TRUE,
                                  control_surv = tern::control_surv_timepoint(conf_level = 0.95),
                                  control_coxph_pw = tern::control_coxph(conf_level = 0.95,
                                                                         pval_method = "log-rank", ties = "efron"),
                                  ci_ribbon = FALSE, vp = grid::viewport(layout.pos.row = nrow_i,
                                                                         layout.pos.col = 1), draw = TRUE)
                     }
                   }, SIMPLIFY = FALSE)
  km_grobs <- tern::stack_grobs(grobs = result)
  km_grobs
}

mapply(eval, mycode)
