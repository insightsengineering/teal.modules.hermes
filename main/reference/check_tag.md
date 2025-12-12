# Check for Shiny Tag

Check whether `x` is a shiny tag.

## Usage

``` r
check_tag(x, null.ok = FALSE)

assert_tag(x, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL)

test_tag(x, null.ok = FALSE)

expect_tag(x, null.ok = FALSE, info = NULL, label = vname(x))
```

## Arguments

- x:

  an object to check.

- null.ok:

  (`flag`)  
  whether `x` may also be `NULL`.

- .var.name:

  (`string`)  
  name of the checked object to print in assertions; defaults to the
  heuristic implemented in
  [`checkmate::vname()`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  (`AssertCollection` or `NULL`)  
  collection to store assertion messages, see
  [`checkmate::AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

- info:

  (`string`)  
  extra information to be included in the message for the `testthat`
  reporter, see
  [`testthat::expect_that()`](https://testthat.r-lib.org/reference/expect_that.html).

- label:

  (`string`)  
  name of the checked object to print in messages. Defaults to the
  heuristic implemented in
  [`checkmate::vname()`](https://mllg.github.io/checkmate/reference/vname.html).

## See also

[`assertions`](https://insightsengineering.github.io/teal.modules.hermes/reference/assertions.md)
for more details.

## Examples

``` r
check_tag("bla")
#> [1] "Must be a 'shiny.tag' or NULL"
check_tag(NULL, null.ok = TRUE)
#> [1] TRUE
```
