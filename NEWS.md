# teal.modules.hermes 0.1.5.9008

### Miscellaneous
* Added placeholders for `assaySpec`, `adtteSpec` and `geneSpec` inputs when no option is selected.
* Disabled the select input for `assaySpec` and `adtteSpec` when there are no options available.

### Enhancements

* Updated the documentation and vignettes with the new way of specifying data for `teal::init()`. The `data` argument accepts `teal_data` object.

# teal.modules.hermes 0.1.5

### Bug Fixes
* Filtering data with the filter panel now correctly triggers the `sampleVarSpecModule`.

### Breaking changes
* Replaced `datasets` argument containing `FilteredData` with the new arguments `data` (`tdata` object) and `filter_panel_api` (`FilterPanelAPI`).

### Miscellaneous
* Removed `scda` from dependencies.
* Migrate to `shinytest2`.

# teal.modules.hermes 0.1.4

* Added the `teal.reporter` functionality to all modules.
* Add installation instruction

# teal.modules.hermes 0.1.3

* Improve the selection of sample variables in the forest module (`tm_g_forest_tte`) such that only categorical variables can be selected in the first place.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Updated package authors.
* Added the option `categorical_only` to the `sampleVarSpec` server module, which allows to only show categorical sample variables for selection.

# teal.modules.hermes 0.1.2

* Rewrote modules to use `moduleServer` and updated call to `plot_with_settings_srv` after changes in `teal.devel`.
* Replaced calls to `teal::root_modules` with `teal::modules` following deprecation of `teal::root_modules`.
* Added basic logging to the modules.
* Fixed the `geneSpecInput` so that the `hermes` app doesn't fail anymore (on chrome) on an experiment with no genes.

# teal.modules.hermes 0.1.1

### Bug Fixes
* Updated legend for the PCA plot.
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
* `tm_g_km` is a `Kaplan-Meier` plot module to analyze RNA-seq gene expression data together with survival data.
* `tm_g_pca` is a principal components analysis plot module for RNA-seq gene expression analysis.
* `tm_g_quality` is a quality control module for RNA-seq gene expression data.
* `tm_g_scatterplot` is a scatterplot module for RNA-seq gene expression analysis.
* `tm_g_volcanoplot` is a differential gene expression analysis module.
