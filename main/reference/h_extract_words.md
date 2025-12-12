# Helper Function to Extract Words

This helper function extracts words from a string. Here words are
defined as containing lower or upper case letters, colons and dots. All
other characters are considered separators.

## Usage

``` r
h_extract_words(x)
```

## Arguments

- x:

  (`string`)  
  input.

## Value

Character vector with the extracted words.

## Examples

``` r
h_extract_words("a, b, , c, 234; 34562 - GeneID:bla")
#> [1] "a"          "b"          "c"          "234"        "34562"     
#> [6] "GeneID:bla"
h_extract_words("GeneID:1820, sdf.393; 32596")
#> [1] "GeneID:1820" "sdf.393"     "32596"      
```
