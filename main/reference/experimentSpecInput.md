# Module Input for Experiment Specification

This defines the input for the experiment specification.

## Usage

``` r
experimentSpecInput(
  inputId,
  data,
  mae_name,
  label_experiments = "Select Experiment"
)
```

## Arguments

- inputId:

  (`string`)  
  the ID used to call the module input.

- data:

  (`reactive`)  
  `reactive(<teal_data>)` holding all the data sets provided during app
  initialization after going through the filters.

- mae_name:

  (`string`)  
  name of the MAE data used in the teal module.

- label_experiments:

  (`string`)  
  label for the experiment selection.

## Value

The UI part.

## See also

[`experimentSpecServer()`](https://insightsengineering.github.io/teal.modules.hermes/reference/experimentSpecServer.md)
for the module server and a complete example.
