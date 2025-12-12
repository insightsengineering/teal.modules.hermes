# Helper Function to Parse Genes

This helper function takes a vector of `words` and tries to match them
with the `id` and `name` columns of possible gene choices.

## Usage

``` r
h_parse_genes(words, choices)
```

## Arguments

- words:

  (`character`)  
  containing gene IDs or names.

- choices:

  (`data.frame`)  
  containing `id` and `name` columns of the new choices.

## Value

The subset of `choices` which matches `words` in ID or name.

## Examples

``` r
h_parse_genes(
  c("a", "2535"),
  data.frame(id = as.character(2533:2537), name = letters[1:5])
)
#>     id name
#> 1 2533    a
#> 3 2535    c
```
