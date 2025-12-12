# Helper Function For Group List Creation

This helper function takes an assignment list and converts it to a group
list.

## Usage

``` r
h_assign_to_group_list(x)
```

## Arguments

- x:

  (named `list` of `character`)  
  input assignment list.

## Value

A combination list.

## Examples

``` r
assign_list <- list(
  "ASIAN" = "1",
  "BLACK OR AFRICAN AMERICAN" = "1",
  "MULTIPLE" = "2",
  "UNKNOWN" = "2",
  "WHITE" = "4"
)
objective_list <- list(
  "ASIAN/BLACK OR AFRICAN AMERICAN" = c("ASIAN", "BLACK OR AFRICAN AMERICAN"),
  "MULTIPLE/UNKNOWN" = c("MULTIPLE", "UNKNOWN"),
  "WHITE" = "WHITE"
)
result_list <- h_assign_to_group_list(assign_list)
stopifnot(identical(result_list, objective_list))
```
