# Most Expressed Genes Plot

This function plots the most expressed genes.

## Usage

``` r
top_gene_plot(object, assay_name)
```

## Arguments

- object:

  (`AnyHermesData`)  
  contains RNA-seq values for one experiment.

- assay_name:

  (`string`)  
  the assay to define the groups.

## Value

Plot to be displayed in the teal app.

## Examples

``` r
library(hermes)
object <- HermesData(summarized_experiment)
result <- top_gene_plot(object, assay_name = "counts")
```
