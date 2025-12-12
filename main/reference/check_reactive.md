# Check for Reactive Input

Check whether `x` is a reactive input.

## Usage

``` r
check_reactive(x)

assert_reactive(x, .var.name = checkmate::vname(x), add = NULL)

test_reactive(x)
```

## Arguments

- x:

  an object to check.

- .var.name:

  (`string`)  
  name of the checked object to print in assertions; defaults to the
  heuristic implemented in
  [`checkmate::vname()`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  (`AssertCollection` or `NULL`)  
  collection to store assertion messages, see
  [`checkmate::AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## See also

[`assertions`](https://insightsengineering.github.io/teal.modules.hermes/reference/assertions.md)
for more details.

## Examples

``` r
check_reactive("bla")
#> [1] FALSE
check_reactive(reactive("bla"))
#> [1] TRUE
```
