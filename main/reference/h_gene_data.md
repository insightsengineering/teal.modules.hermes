# Helper Function to Format Gene Choices

Given a
[`hermes::AnyHermesData`](https://insightsengineering.github.io/hermes/latest-tag/reference/HermesData-class.html)
data object, as well as the annotation column name to use as gene name,
this function formats the contained genes as a `data.frame` ready for
consumption in
[`h_order_genes()`](https://insightsengineering.github.io/teal.modules.hermes/reference/h_order_genes.md)
e.g.

## Usage

``` r
h_gene_data(object, name_annotation)
```

## Arguments

- object:

  (`AnyHermesData`)  
  contains RNA-seq values for one experiment.

- name_annotation:

  (`string` or `NULL`)  
  which annotation column to use as name to return in the `genes` data.
  If `NULL`, then the `name` column will be set to empty strings.

## Value

A `data.frame` with `id` and `name` columns containing all genes from
`object`.

## Details

Note that missing names or names that only contain whitespace are
replaced by empty strings for consistency and better labeling in the UI
downstream

## Examples

``` r
object <- hermes::hermes_data[1:10, ]
h_gene_data(object, "symbol")
#>                  id         name
#> 1      GeneID:11185         INMT
#> 2      GeneID:10677         AVIL
#> 3  GeneID:101928428 LOC101928428
#> 4  GeneID:100422835      MIR3183
#> 5  GeneID:102466731     MIR6769A
#> 6      GeneID:64881       PCDH20
#> 7     GeneID:286205         SCAI
#> 8       GeneID:8365     HIST1H4H
#> 9       GeneID:6804        STX1A
#> 10 GeneID:100423018    MIR3156-3
```
