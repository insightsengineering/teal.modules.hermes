# Data Preprocessing for `ADTTE` Module

A function to help with merging of MAE to `ADTTE`.

## Usage

``` r
h_km_mae_to_adtte(
  adtte,
  mae,
  genes,
  experiment_name = "hd1",
  assay_name = "counts",
  usubjid_var = "USUBJID"
)
```

## Arguments

- adtte:

  (`data frame`)  
  an `adtte` dataset.

- mae:

  (`MultiAssayExperiment`)  
  contains `AnyHermesData` objects.

- genes:

  (`GeneSpec`)  
  specification for gene(s) (signature), e.g. using
  [`hermes::gene_spec()`](https://insightsengineering.github.io/hermes/latest-tag/reference/gene_spec.html).

- experiment_name:

  (`string`)  
  the desired `HermesData` to use.

- assay_name:

  (`string`)  
  the assay to define the groups.

- usubjid_var:

  (`string`)  
  variable name of the subject ID variable.

## Value

A data frame containing all columns/rows from `adtte` that match by
subject ID with the row names of the MAE and have the gene samples
available in the given experiment. The attribute `gene_cols` contains
the column names for the gene columns.

## Note

The final gene column names can start with a different string than the
original gene IDs (or labels), in particular white space and colons are
removed.

## Examples

``` r
library(dplyr)
mae <- hermes::multi_assay_experiment
adtte <- teal.data::rADTTE %>%
  dplyr::mutate(CNSR = as.logical(CNSR))

new_adtte <- h_km_mae_to_adtte(
  adtte,
  mae,
  genes = hermes::gene_spec("GeneID:1820"),
  experiment_name = "hd2"
)
new_adtte2 <- h_km_mae_to_adtte(
  adtte,
  mae,
  genes = hermes::gene_spec(c("GeneID:1820", "GeneID:94115"), fun = colMeans),
  experiment_name = "hd2"
)
new_adtte3 <- h_km_mae_to_adtte(
  adtte,
  mae,
  genes = hermes::gene_spec(c(A = "GeneID:1820", B = "GeneID:94115")),
  experiment_name = "hd2"
)
```
