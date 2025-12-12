# Checking for Empty String

This predicate function is helpful for functions where arguments could
not yet be initialized from the teal module.

## Usage

``` r
is_blank(x)
```

## Arguments

- x:

  object to check.

## Value

Flag whether `x` is identical to an empty string, i.e. `""`.

## Examples

``` r
is_blank("")
#> [1] TRUE
is_blank(" ")
#> [1] FALSE
```
