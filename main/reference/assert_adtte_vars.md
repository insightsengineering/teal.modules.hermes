# Check for `ADTTE` Variables

Check whether `x` is a list of `ADTTE` variables.

## Usage

``` r
assert_adtte_vars(x)
```

## Arguments

- x:

  an object to check.

## See also

[`assertions`](https://insightsengineering.github.io/teal.modules.hermes/reference/assertions.md)
for more details.

## Examples

``` r
assert_adtte_vars(list(aval = "AV", is_event = "EV", paramcd = "PC", usubjid = "ID", avalu = "u"))
```
