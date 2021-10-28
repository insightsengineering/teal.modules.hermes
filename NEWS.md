# teal.modules.hermes 0.1.0.9000

### Bug Fixes
* Gene selection with more than a few thousand genes no longer hangs the application. This is achieved through a different selection input in the corresponding `geneSpec` shiny module.

### Miscellaneous
* Updated R version requirement to >= 3.6.

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
