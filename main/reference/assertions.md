# Additional Assertions for `checkmate`

We provide additional assertion functions which can be used together
with the `checkmate` functions. These are described in individual help
pages linked below.

## Value

Depending on the function prefix.

- `assert_` functions return the object invisibly if successful, and
  otherwise throw an error message.

- `check_` functions return `TRUE` if successful, otherwise a string
  with the error message.

- `test_` functions just return `TRUE` or `FALSE`.

## See also

[`assert_tag()`](https://insightsengineering.github.io/teal.modules.hermes/reference/check_tag.md),
[`assert_reactive()`](https://insightsengineering.github.io/teal.modules.hermes/reference/check_reactive.md),
[`assert_summary_funs()`](https://insightsengineering.github.io/teal.modules.hermes/reference/assert_summary_funs.md),
[`assert_adtte_vars()`](https://insightsengineering.github.io/teal.modules.hermes/reference/assert_adtte_vars.md)
