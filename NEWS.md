# teal.modules.hermes 0.1.1.9004

* Rewrote modules to use `moduleServer` and updated call to `plot_with_settings_srv` after changes in `teal.devel`.
* Added basic logging to the modules.

# teal.modules.hermes 0.1.1

### Bug Fixes
* Gene selection with more than a few thousand genes no longer hangs the application. This is achieved through a different selection input in the corresponding `geneSpec` shiny module.
* Only atomic columns of `colData` with at least one value can now be selected in the `sampleVarSpecModule`.

### Miscellaneous
* Updated R version requirement to >= 3.6.
* Updated `sampleVarSpec` and `geneSpec` modules with new icons to remove warnings when using shiny version >= 1.7.
* Removed dependencies on deprecated packages `utils.nest` and `test.nest`.

# teal.modules.hermes 0.1.0
* First release of the `teal.modules.hermes` package, which contains teal modules for RNA-seq analysis using the `hermes` package.
* New users should first begin by reading the `README.md` of the `teal.modules.hermes` package to become familiar.

### New Features
* `tm_g_barplot` is a barplot module for RNA-seq gene expression analysis.
* `tm_g_boxplot` is a boxplot module for RNA-seq gene expression analysis.
* `tm_g_forest_tte` is a survival forest plot module to analyze RNA-seq gene expression data together with survival data.
* `tm_g_km` is a Kaplan-Meier plot module to analyze RNA-seq gene expression data together with survival data.
* `tm_g_pca` is a principal components analysis plot module for RNA-seq gene expression analysis.
* `tm_g_quality` is a quality control module for RNA-seq gene expression data.
* `tm_g_scatterplot` is a scatterplot module for RNA-seq gene expression analysis.
* `tm_g_volcanoplot` is a differential gene expression analysis module.
