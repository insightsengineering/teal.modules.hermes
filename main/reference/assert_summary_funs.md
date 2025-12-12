# Check for List of Summary Functions

Check whether `x` is a list of summary functions.

## Usage

``` r
assert_summary_funs(x, null.ok = FALSE)
```

## Arguments

- x:

  an object to check.

- null.ok:

  (`flag`)  
  whether `x` may also contain `NULL`, meaning that a user choice is
  possible where no summary function should be applied.

## See also

[`assertions`](https://insightsengineering.github.io/teal.modules.hermes/reference/assertions.md)
for more details.

## Examples

``` r
assert_summary_funs(list(mean = colMeans, raw = NULL), null.ok = TRUE)
```
