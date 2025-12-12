# Helper Function for Collapsing of Factor Levels

Given a group list and a factor, this helper function collapses the
levels in the factor accordingly and also ensures that the resulting
levels are in the order given in the group list.

## Usage

``` r
h_collapse_levels(x, group_list)
```

## Arguments

- x:

  (`factor`)  
  original factor.

- group_list:

  (named `list` of `character`)  
  includes the collapsing specification.

## Value

The transformed factor `x` with new levels.

## Examples

``` r
set.seed(123)
x <- factor(sample(
  c("ASIAN", "BLACK OR AFRICAN AMERICAN", "MULTIPLE", "UNKNOWN", "WHITE"),
  size = 30L,
  replace = TRUE
))
group_list <- list(
  "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
  "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
  "WHITE" = "WHITE"
)
x_collapsed <- h_collapse_levels(x, group_list)
stopifnot(identical(levels(x_collapsed), names(group_list)))
```
