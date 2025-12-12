# Standard Assertion Arguments

The documentation to this function lists all the conventional arguments
in additional `checkmate` assertions.

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
