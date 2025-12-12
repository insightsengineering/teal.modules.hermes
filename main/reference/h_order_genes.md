# Helper Function to Order Gene Choices

The possible gene choices are ordered as follows. First come all genes
which have a non-empty name, ordered by their name alphabetically. Last
come all genes with an empty name, ordered by their ID alphabetically.

## Usage

``` r
h_order_genes(genes)
```

## Arguments

- genes:

  (`data.frame`)  
  containing `id` and `name` columns of the gene choices. Note that no
  missing values are allowed.

## Value

The ordered `data.frame`.

## Examples

``` r
genes <- data.frame(
  id = c("7", "1", "2", "345346", "0"),
  name = c("e", "", "c", "", "a")
)
h_order_genes(genes)
#>       id name
#> 5      0    a
#> 3      2    c
#> 1      7    e
#> 2      1     
#> 4 345346     
```
