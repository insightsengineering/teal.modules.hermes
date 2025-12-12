# Validation of Gene Specification

This validation function checks that a given
[`hermes::GeneSpec`](https://insightsengineering.github.io/hermes/latest-tag/reference/GeneSpec.html)
has at least one gene selected and that all genes are included in
possible choices.

## Usage

``` r
validate_gene_spec(gene_spec, gene_choices)
```

## Arguments

- gene_spec:

  (`GeneSpec`)  
  gene specification.

- gene_choices:

  (`character`)  
  all possible gene choices.
